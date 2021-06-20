use std::hash::{Hash, Hasher};
use std::any::Any;
use std::fmt;

// An autoimplemented trait, necessary to allow casting to &dyn Any
pub trait AsAny {
	fn as_any(&self) -> &dyn Any;
	fn as_any_mut(&mut self) -> &mut dyn Any;
}
impl<T: Any> AsAny for T {
	fn as_any(&self) -> &dyn Any { self }
	fn as_any_mut(&mut self) -> &mut dyn Any { self }
}


#[derive(Clone, Copy)]
pub struct DynType {
	pub type_id: std::any::TypeId,
	pub type_name: &'static str,
}

impl PartialEq for DynType {
	fn eq(&self, other: &DynType) -> bool {
		self.type_id == other.type_id
	}
}
impl Eq for DynType {}
impl Hash for DynType {
	fn hash<H>(&self, state: &mut H) where H: std::hash::Hasher {
		self.type_id.hash(state)
	}
}


pub trait DynTyped: AsAny {
	fn get_type(&self) -> DynType;
	
	fn get_type_name(&self) -> &str {
		self.get_type().type_name
	}
}

#[macro_export]
macro_rules! register_dyn_type {
	($name:expr, $type:ty) => {
		impl DynTyped for $type {
			fn get_type(&self) -> DynType {
				return DynType {
					type_id: std::any::TypeId::of::<$type>(),
					type_name: $name,
				};
			}
		}
	};
}


// Trait allowing use of Eq and Hash on trait objects
pub trait ImmData: DynTyped {
	fn imm_eq(&self, other: &dyn AsAny) -> bool;
	fn imm_hash(&self) -> u64;
}

impl<T: 'static + Eq + Hash + DynTyped> ImmData for T {
	fn imm_eq(&self, other: &dyn AsAny) -> bool {
		let other_any = other.as_any();
		if !other_any.is::<T>() {
			return false;
		}
		self == other_any.downcast_ref::<T>().unwrap()
	}
	fn imm_hash(&self) -> u64 {
		let mut s = std::collections::hash_map::DefaultHasher::new();
		self.get_type().type_id.hash(&mut s);
		self.hash(&mut s);
		s.finish()
	}
}


#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
	Int, Float, String, Bool, Nil,
	Object(DynType),
	Tuple, List, Map, Struct, Function, Iterator,
}

impl fmt::Display for Type {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		let s = match self {
			Type::Int => "int",
			Type::Float => "float",
			Type::String => "str",
			Type::Bool => "bool",
			Type::Nil => "nil",
			
			Type::Object(t) => t.type_name,
			
			Type::Tuple => "tuple",
			Type::List => "list",
			Type::Map => "map",
			Type::Struct => "struct",
			Type::Function => "fun",
			Type::Iterator => "iter",
		};
		write!(fmt, "{}", s)
	}
}

pub fn expected(exp: &str, got: &str) -> String {
	format!("Expected {}, got {}", exp, got)
}


