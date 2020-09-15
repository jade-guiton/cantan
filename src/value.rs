use std::fmt::Write;
use std::ops::Deref;

use gc_arena::Gc;

use crate::ast::Primitive;

#[derive(Debug, Clone, Collect)]
#[collect(no_drop)]
pub enum Value<'gc> {
	Primitive(Primitive),
	List(Gc<'gc, Vec<Value<'gc>>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
	Int, Float, String, Bool, Nil, List,
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
			Value::List(_) => Type::List,
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

impl Value<'_> {
	get_prim!(get_bool, bool, Bool);
	get_prim!(get_int, i32, Int);
	get_prim!(get_string, String, String);
	pub fn get_numeric(&self) -> Result<f64, String> {
		match self {
			Value::Primitive(Primitive::Int(i)) => Ok(*i as f64),
			Value::Primitive(Primitive::Float(f)) => Ok(*f),
			_ => {
				expected_types("Int or Float", self)
			}
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
			Value::List(list) => {
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
			}
		}
	}
	
	pub fn eq(&self, other: &Value) -> bool {
		match self {
			Value::Primitive(p1) =>
				if let Value::Primitive(p2) = other { p1 == p2 } else { false },
			Value::List(l1) =>
				if let Value::List(l2) = other {
					if l1.len() == l2.len() {
						l1.iter().zip(l2.deref()).all(|(a,b)| a.eq(b))
					} else { false }
				} else { false },
		}
	}
}

impl From<&Primitive> for Value<'_> {
	fn from(cst: &Primitive) -> Self {
		Value::Primitive(cst.clone())
	}
}
