use gc_arena::Collect;
use std::hash::{Hash, Hasher};
use std::collections::HashMap;
use std::fmt::Write;
use std::ops::Deref;

use gc_arena::Gc;

use crate::ast::Primitive;

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
	List(Gc<'gc, Vec<Value<'gc>>>),
	Map(Gc<'gc, HashMap<Value<'gc>, Value<'gc>>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
	Int, Float, String, Bool, Nil, Tuple, List, Map,
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
			(Value::List(l1), Value::List(l2)) =>
				if l1.len() == l2.len() {
					l1.iter().zip(l2.deref()).all(|(a,b)| a.struct_eq(b))
				} else { false },
			(Value::Map(m1), Value::Map(m2)) => // Memory equality for keys, structural for values
				if m1.len() == m2.len() {
					m1.iter().all(|(k,v)| m2.get(k).map_or(false, |v2| v.struct_eq(v2)))
				} else { false }
			_ => false,
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
				let mut buf = String::new();
				write!(buf, "[").unwrap();
				if map.len() == 0 {
					write!(buf, ":").unwrap();
				} else {
					for (idx, (key, val)) in map.iter().enumerate() {
						write!(buf, "{}: {}", key.repr(), val.repr()).unwrap();
						if idx < map.len() - 1 {
							write!(buf, ", ").unwrap();
						}
					}
				}
				write!(buf, "]").unwrap();
				buf
			},
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
			(Value::List(l1), Value::List(l2)) => Gc::as_ptr(*l1) == Gc::as_ptr(*l2),
			(Value::Map(m1), Value::Map(m2)) => Gc::as_ptr(*m1) == Gc::as_ptr(*m2),
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
			Value::List(list) => Gc::as_ptr(*list).hash(state),
			Value::Map(map) => Gc::as_ptr(*map).hash(state),
		}
	}
}

impl From<&Primitive> for Value<'_> {
	fn from(cst: &Primitive) -> Self {
		Value::Primitive(cst.clone())
	}
}
