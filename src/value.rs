use gc_arena::Collect;
use std::hash::{Hash, Hasher};
use std::convert::TryFrom;
use std::collections::HashMap;
use std::cmp::Ordering;
use std::fmt::Write;
use std::ops::Deref;
use std::rc::Rc;

use gc_arena::{Gc, GcCell, MutationContext};
use ordered_float::NotNan;

use crate::ast::Primitive;
use crate::chunk::CompiledFunction;

// To resolve infinite recursion from default impl
unsafe impl Collect for Tuple<'_> {
	fn needs_trace() -> bool { true }
}

#[derive(Collect)]
#[collect(no_drop)]
pub enum Upvalue<'gc> {
	Open(usize),
	Closed(Value<'gc>),
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct Function<'gc> {
	pub chunk: Rc<CompiledFunction>,
	pub upvalues: Vec<GcCell<'gc, Upvalue<'gc>>>,
}

impl<'gc> Function<'gc> {
	pub fn main(chunk: Rc<CompiledFunction>) -> Self {
		Function { chunk, upvalues: vec![] }
	}
}

// We need a new type to allow us to override the default
// Collect implementation on Boxed slices, which currently
// causes infinite recursion
#[derive(Clone, Debug)]
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

// New type to implement Collect
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct NiceFloat(pub NotNan<f64>);

impl Deref for NiceFloat {
	type Target = f64;
	fn deref(&self) -> &f64 {
		self.0.deref()
	}
}

unsafe impl Collect for NiceFloat {
	fn needs_trace() -> bool { false }
}

// New type to implement Collect
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct NiceStr(pub Box<str>);

impl From<String> for NiceStr {
	fn from(s: String) -> Self {
		NiceStr(s.into_boxed_str())
	}
}

impl Deref for NiceStr {
	type Target = str;
	fn deref(&self) -> &str {
		self.0.deref()
	}
}

unsafe impl Collect for NiceStr {
	fn needs_trace() -> bool { false }
}

#[derive(Clone, Debug, Collect)]
#[collect(no_drop)]
pub enum Value<'gc> {
	// Unpacked Primitive
	Nil,
	Bool(bool),
	Int(i32),
	Float(NiceFloat),
	String(NiceStr),
	
	Tuple(Tuple<'gc>),
	List(GcCell<'gc, Vec<Value<'gc>>>),
	Map(GcCell<'gc, HashMap<Value<'gc>, Value<'gc>>>),
	Object(GcCell<'gc, HashMap<String, Value<'gc>>>),
	Function(Gc<'gc, Function<'gc>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
	Int, Float, String, Bool, Nil, Tuple, List, Map, Object, Function,
}

impl Value<'_> {
	pub fn get_type(&self) -> Type {
		match self {
			Value::Nil => Type::Nil,
			Value::Bool(_) => Type::Bool,
			Value::Int(_) => Type::Int,
			Value::Float(_) => Type::Float,
			Value::String(_) => Type::String,
			
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
			if let Value::$cn_type(b) = self {
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
	get_prim!(get_string, NiceStr, String);
	
	// We can't use .clone() for this, because it keeps lifetimes
	// intact, and we need to change from 'static to 'gc when
	// instantiating a constant.
	pub fn clone_prim<'gc2>(&self) -> Value<'gc2> {
		match self {
			Value::Nil => Value::Nil,
			Value::Bool(b) => Value::Bool(*b),
			Value::Int(i) => Value::Int(*i),
			Value::Float(f) => Value::Float(f.clone()),
			Value::String(s) => Value::String(s.clone()),
			_ => panic!("Trying to move non-primitive between GCs"),
		}
	}
	
	pub fn get_numeric(&self) -> Result<f64, String> {
		match self {
			Value::Int(i) => Ok(*i as f64),
			Value::Float(f) => Ok(**f),
			_ => {
				expected_types("Int or Float", self)
			}
		}
	}
	
	pub fn struct_eq(&self, other: &Value<'gc>) -> bool {
		match (self, other) {
			(Value::Nil, Value::Nil) => true,
			(Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
			(Value::Int(i1), Value::Int(i2)) => i1 == i2,
			(Value::Float(f1), Value::Float(f2)) => f1.0 == f2.0,
			(Value::String(s1), Value::String(s2)) => s1.0 == s2.0,
			
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
			(Value::Int(i1), Value::Int(i2)) =>
				Ok(i1.cmp(i2)),
			(Value::Int(i1), Value::Float(f2)) =>
				Ok(NotNan::new(*i1 as f64).unwrap().cmp(&f2.0)),
			(Value::Float(f1), Value::Int(i2)) =>
				Ok(f1.0.cmp(&NotNan::new(*i2 as f64).unwrap())),
			(Value::Float(f1), Value::Float(f2)) =>
				Ok(f1.0.cmp(&f2.0)),
			(Value::String(s1), Value::String(s2)) =>
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
			Value::Nil => String::from("nil"),
			Value::Bool(b) => format!("{:?}", b),
			Value::Int(i) => format!("{}", i),
			Value::Float(f) => format!("{}", f.0),
			Value::String(s) => format!("{:?}", s.0),
			
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
				format!("<function {:x}>", Gc::as_ptr(*fun) as usize)
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
			(Value::Nil, Value::Nil) => true,
			(Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
			(Value::Int(i1), Value::Int(i2)) => i1 == i2,
			(Value::Float(f1), Value::Float(f2)) => f1.0 == f2.0,
			(Value::String(s1), Value::String(s2)) => s1.0 == s2.0,
			
			(Value::Tuple(t1), Value::Tuple(t2)) => **t1 == **t2,
			(Value::List(l1), Value::List(l2)) => l1.as_ptr() == l2.as_ptr(),
			(Value::Map(m1), Value::Map(m2)) => m1.as_ptr() == m2.as_ptr(),
			(Value::Object(o1), Value::Object(o2)) => o1.as_ptr() == o2.as_ptr(),
			(Value::Function(f1), Value::Function(f2)) => Gc::as_ptr(*f1) == Gc::as_ptr(*f2),
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
			Value::Nil => {},
			Value::Bool(b) => b.hash(state),
			Value::Int(i) => i.hash(state),
			Value::Float(f) => f.0.hash(state),
			Value::String(s) => s.0.hash(state),
			
			Value::Tuple(values) => values.hash(state),
			Value::List(list) => list.as_ptr().hash(state),
			Value::Map(map) => map.as_ptr().hash(state),
			Value::Object(obj) => obj.as_ptr().hash(state),
			Value::Function(fun) => Gc::as_ptr(*fun).hash(state),
		}
	}
}

impl From<&Primitive> for Value<'_> {
	fn from(cst: &Primitive) -> Self {
		match cst {
			Primitive::Nil => Value::Nil,
			Primitive::Bool(b) => Value::Bool(*b),
			Primitive::Int(i) => Value::Int(*i),
			Primitive::Float(f) => Value::Float(NiceFloat(*f)),
			Primitive::String(s) => Value::String(NiceStr(s.clone())),
		}
	}
}

impl TryFrom<f64> for Value<'_> {
	type Error = String;
	fn try_from(f: f64) -> Result<Self, String> {
		if f.is_finite() {
			Ok(Value::Float(NiceFloat(NotNan::new(f).unwrap())))
		} else {
			Err(String::from("Float operation had Infinite or NaN result"))
		}
	}
}
