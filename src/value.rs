use gc_arena::Collect;
use std::hash::{Hash, Hasher};
use std::convert::TryFrom;
use std::collections::HashMap;
use std::cmp::Ordering;
use std::fmt::Write;
use std::ops::Deref;
use std::rc::Rc;

use gc_arena::{GcCell, MutationContext};
use ordered_float::NotNan;

use crate::ast::Primitive;
use crate::chunk::CompiledFunction;

// We need a new type to allow us to override the default
// Collect implementation on Boxed slices, which currently
// causes infinite recursion
#[derive(Clone)]
pub struct Tuple<'gc>(Box<[Value<'gc>]>);

impl<'gc> Tuple<'gc> {
	pub fn new(values: Vec<Value<'gc>>) -> Self {
		Tuple(values.into_boxed_slice())
	}
}

impl<'gc> Deref for Tuple<'gc> {
	type Target = [Value<'gc>];
	fn deref(&self) -> &[Value<'gc>] { self.0.deref() }
}

// To resolve infinite recursion from default impl
unsafe impl Collect for Tuple<'_> {
	fn needs_trace() -> bool { true }
}

#[derive(Clone, Collect)]
#[collect(no_drop)]
pub enum Value<'gc> {
	Primitive(Primitive),
	Tuple(Tuple<'gc>),
	List(GcCell<'gc, Vec<Value<'gc>>>),
	Map(GcCell<'gc, HashMap<Value<'gc>, Value<'gc>>>),
	Object(GcCell<'gc, HashMap<String, Value<'gc>>>),
	Function(Rc<CompiledFunction>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
	Int, Float, String, Bool, Nil, Tuple, List, Map, Object, Function,
}

impl Value<'_> {
	pub fn get_type(&self) -> Type {
		match self {
			Value::Primitive(prim) => {
				match prim {
					Primitive::Int(_) => Type::Int,
					Primitive::Float(_) => Type::Float,
					Primitive::String(_) => Type::String,
					Primitive::Bool(_) => Type::Bool,
					Primitive::Nil => Type::Nil,
				}
			},
			Value::Tuple(_) => Type::Tuple,
			Value::List(_) => Type::List,
			Value::Map(_) => Type::Map,
			Value::Object(_) => Type::Object,
			Value::Function(_) => Type::Function,
		}
	}
}

pub fn expected<T>(exp: &str, got: &str) -> Result<T, String> {
	Err(format!("Expected {}, got {}", exp, got))
}

pub fn expected_types<T>(exp: &str, got: &Value) -> Result<T, String> {
	expected(exp, &format!("{:?}", got.get_type()))
}

pub fn expected_type<T>(exp: Type, got: &Value) -> Result<T, String> {
	expected_types(&format!("{:?}", exp), got)
}

macro_rules! get_prim {
	($name:ident, $rust_type:ty, $cn_type:ident) => {
		pub fn $name(&self) -> Result<$rust_type, String> {
			if let Value::Primitive(Primitive::$cn_type(b)) = self {
				Ok(b.clone())
			} else {
				expected_type(Type::$cn_type, self)
			}
		}
	};
}

impl<'gc> Value<'gc> {
	get_prim!(get_bool, bool, Bool);
	get_prim!(get_int, i32, Int);
	get_prim!(get_string, String, String);
	
	pub fn get_numeric(&self) -> Result<f64, String> {
		match self {
			Value::Primitive(Primitive::Int(i)) => Ok(*i as f64),
			Value::Primitive(Primitive::Float(f)) => Ok(**f),
			_ => {
				expected_types("Int or Float", self)
			}
		}
	}
	
	pub fn struct_eq(&self, other: &Value<'gc>) -> bool {
		match (self, other) {
			(Value::Primitive(p1), Value::Primitive(p2)) => p1 == p2,
			(Value::Tuple(t1), Value::Tuple(t2)) =>
				if t1.len() == t2.len() {
					t1.iter().zip(t2.deref()).all(|(a,b)| a.struct_eq(b))
				} else { false },
			(Value::List(l1), Value::List(l2)) => {
				let (l1, l2) = (l1.read(), l2.read());
				if l1.len() == l2.len() {
					l1.iter().zip(l2.deref()).all(|(a,b)| a.struct_eq(b))
				} else { false }
			},
			(Value::Map(m1), Value::Map(m2)) => { // Memory equality for keys, structural for values
				let (m1, m2) = (m1.read(), m2.read());
				if m1.len() == m2.len() {
					m1.iter().all(|(k,v)| m2.get(k).map_or(false, |v2| v.struct_eq(v2)))
				} else { false }
			},
			(Value::Object(o1), Value::Object(o2)) => {
				let (o1, o2) = (o1.read(), o2.read());
				if o1.len() == o2.len() {
					o1.iter().all(|(k,v)| o2.get(k).map_or(false, |v2| v.struct_eq(v2)))
				} else { false }
			},
			_ => false,
		}
	}
	
	pub fn cmp(&self, other: &Value<'gc>) -> Result<Ordering, String> {
		match (self, other) {
			(Value::Primitive(Primitive::Int(i1)), Value::Primitive(Primitive::Int(i2))) =>
				Ok(i1.cmp(i2)),
			(Value::Primitive(Primitive::Int(i1)), Value::Primitive(Primitive::Float(f2))) =>
				Ok(NotNan::new(*i1 as f64).unwrap().cmp(f2)),
			(Value::Primitive(Primitive::Float(f1)), Value::Primitive(Primitive::Int(i2))) =>
				Ok(f1.cmp(&NotNan::new(*i2 as f64).unwrap())),
			(Value::Primitive(Primitive::Float(f1)), Value::Primitive(Primitive::Float(f2))) =>
				Ok(f1.cmp(f2)),
			(Value::Primitive(Primitive::String(s1)), Value::Primitive(Primitive::String(s2))) =>
				Ok(s1.cmp(s2)),
			(Value::Tuple(t1), Value::Tuple(t2)) => {
				if t1.len() == t2.len() {
					let o = t1.iter().zip(t2.iter()).find_map(|(v1,v2)| {
						let res = v1.cmp(v2);
						if let Ok(Ordering::Equal) = res {
							None
						} else {
							Some(res)
						}
					});
					match o {
						Some(res) => res,
						None => Ok(Ordering::Equal),
					}
				} else {
					Err(format!("Cannot compare {}-tuple and {}-tuple", t1.len(), t2.len()))
				}
			}
			_ => Err(format!("Cannot compare: '{}', '{}'", self.repr(), other.repr()))
		}
	}
	
	pub fn repr(&self) -> String {
		match self {
			Value::Primitive(prim) => match prim {
				Primitive::Int(i) => format!("{}", i),
				Primitive::Float(f) => format!("{}", f),
				Primitive::String(s) => format!("{:?}", s),
				Primitive::Bool(b) => format!("{:?}", b),
				Primitive::Nil => String::from("nil"),
			},
			Value::Tuple(list) => {
				let mut buf = String::new();
				write!(buf, "[").unwrap();
				for (idx, val) in list.iter().enumerate() {
					write!(buf, "{}", val.repr()).unwrap();
					if idx < list.len() - 1 {
						write!(buf, ", ").unwrap();
					}
				}
				write!(buf, "]").unwrap();
				buf
			},
			Value::List(list) => {
				let list = list.read();
				let mut buf = String::new();
				write!(buf, "[|").unwrap();
				for (idx, val) in list.iter().enumerate() {
					write!(buf, "{}", val.repr()).unwrap();
					if idx < list.len() - 1 {
						write!(buf, ", ").unwrap();
					}
				}
				write!(buf, "]").unwrap();
				buf
			},
			Value::Map(map) => {
				let map = map.read();
				let mut buf = String::new();
				write!(buf, "[").unwrap();
				if map.len() == 0 {
					write!(buf, "=").unwrap();
				} else {
					for (idx, (key, val)) in map.iter().enumerate() {
						write!(buf, "{}={}", key.repr(), val.repr()).unwrap();
						if idx < map.len() - 1 {
							write!(buf, ", ").unwrap();
						}
					}
				}
				write!(buf, "]").unwrap();
				buf
			},
			Value::Object(obj) => {
				let obj = obj.read();
				let mut buf = String::new();
				write!(buf, "{{").unwrap();
				for (idx, (key, val)) in obj.iter().enumerate() {
					write!(buf, "{}={}", key, val.repr()).unwrap();
					if idx < obj.len() - 1 {
						write!(buf, ", ").unwrap();
					}
				}
				write!(buf, "}}").unwrap();
				buf
			},
			Value::Function(fun) => {
				format!("<function {:x}>", Rc::as_ptr(fun) as usize)
			}
		}
	}
	
	pub fn index(&self, idx: &Value<'gc>) -> Result<Value<'gc>, String> {
		match self {
			Value::Tuple(tuple) => {
				let idx = idx.get_int()?;
				usize::try_from(idx).ok().and_then(|idx| tuple.get(idx)).cloned()
					.ok_or_else(|| format!("Trying to index {}-tuple with: {}", tuple.len(), idx))
			},
			Value::List(list) => {
				let list = list.read();
				let idx = idx.get_int()?;
				usize::try_from(idx).ok().and_then(|idx| list.get(idx)).cloned()
					.ok_or_else(|| format!("Trying to index list of length {} with: {}", list.len(), idx))
			},
			Value::Map(map) => {
				Ok(map.read().get(idx).ok_or_else(|| format!("Map does not contain key: {}", idx.repr()))?.clone())
			},
			_ => {
				Err(format!("Cannot get index from {:?}", self.get_type()))
			}
		}
	}
	
	pub fn set_index(&self, idx: &Value<'gc>, val: Value<'gc>, mc: MutationContext<'gc, '_>) -> Result<(), String> {
		match self {
			Value::List(list) => {
				let mut list = list.write(mc);
				let idx = idx.get_int()?;
				let len = list.len();
				let slot = usize::try_from(idx).ok().and_then(|idx| list.get_mut(idx))
					.ok_or_else(|| format!("Trying to index list of length {} with: {}", len, idx))?;
				*slot = val;
			},
			Value::Map(map) => {
				let mut map = map.write(mc);
				let slot = map.get_mut(idx).ok_or_else(|| format!("Map does not contain key: {}", idx.repr()))?;
				*slot = val;
			},
			_ => return Err(format!("Cannot set index in {:?}", self.get_type())),
		}
		Ok(())
	}
	
	pub fn prop(&self, prop: &str) -> Result<Value<'gc>, String> {
		match self {
			Value::Object(obj) => {
				Ok(obj.read().get(prop).ok_or_else(|| format!("Object does not have prop '{}'", prop))?.clone())
			},
			_ => {
				Err(format!("Cannot get prop of {:?}", self.get_type()))
			}
		}
	}
	
	pub fn set_prop(&self, prop: &str, val: Value<'gc>, mc: MutationContext<'gc, '_>) -> Result<(), String> {
		match self {
			Value::Object(obj) => {
				let mut obj = obj.write(mc);
				let slot = obj.get_mut(prop).ok_or_else(|| format!("Object does not have prop '{}'", prop))?;
				*slot = val;
				Ok(())
			},
			_ => {
				Err(format!("Cannot get prop of {:?}", self.get_type()))
			}
		}
	}
}

// Note: This is memory equality, not structural identity;
// The "==" operator in-language uses struct_eq() instead.
// This is essentially used in the implementation of maps.
impl<'gc> PartialEq for Value<'gc> {
	fn eq(&self, other: &Value<'gc>) -> bool {
		match (self, other) {
			(Value::Primitive(p1), Value::Primitive(p2)) => p1 == p2,
			(Value::Tuple(t1), Value::Tuple(t2)) => **t1 == **t2,
			(Value::List(l1), Value::List(l2)) => l1.as_ptr() == l2.as_ptr(),
			(Value::Map(m1), Value::Map(m2)) => m1.as_ptr() == m2.as_ptr(),
			_ => false,
		}
	}
}
impl Eq for Value<'_> {}

// Reminder: 
impl Hash for Value<'_> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.get_type().hash(state);
		match self {
			Value::Primitive(prim) => prim.hash(state),
			Value::Tuple(values) => values.hash(state),
			Value::List(list) => list.as_ptr().hash(state),
			Value::Map(map) => map.as_ptr().hash(state),
			Value::Object(obj) => obj.as_ptr().hash(state),
			Value::Function(fun) => Rc::as_ptr(fun).hash(state),
		}
	}
}

impl From<&Primitive> for Value<'_> {
	fn from(cst: &Primitive) -> Self {
		Value::Primitive(cst.clone())
	}
}
