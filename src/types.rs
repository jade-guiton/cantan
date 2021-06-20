use std::hash::Hash;
use std::any::Any;
use std::fmt;

// An autoimplemented trait, necessary to allow casting to &dyn Any
pub trait AsAny {
	fn as_any(&self) -> &dyn Any;
	fn as_any_mut(&mut self) -> &mut dyn Any;
}


#[derive(Clone, Copy, Eq)]
pub struct DynType {
	pub type_id: std::any::TypeId,
	pub type_name: &'static str,
}

impl PartialEq for DynType {
	fn eq(&self, other: &DynType) -> bool {
		self.type_id == other.type_id
	}
}
impl Hash for DynType {
	fn hash<H>(&self, state: &mut H) where H: std::hash::Hasher {
		self.type_id.hash(state)
	}
}


pub trait StaticDynTyped {
	const DYN_TYPE: DynType;
}

pub trait DynTyped: AsAny {
	fn get_type(&self) -> DynType;
}

impl<T: 'static + DynTyped> AsAny for T {
	fn as_any(&self) -> &dyn Any { self }
	fn as_any_mut(&mut self) -> &mut dyn Any { self }
}

#[macro_export]
macro_rules! register_dyn_type {
	($name:expr, $type:ty) => {
		impl StaticDynTyped for $type {
			const DYN_TYPE: DynType = DynType {
				type_id: std::any::TypeId::of::<$type>(),
				type_name: $name,
			};
		}
		
		impl DynTyped for $type {
			fn get_type(&self) -> DynType {
				return <$type as StaticDynTyped>::DYN_TYPE;
			}
		}
	};
}


#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
	Int, Float, String, Bool, Nil,
	Object(DynType),
	Iterator,
}

impl fmt::Display for Type {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		let s = match self {
			Type::Int => "int",
			Type::Float => "float",
			Type::String => "string",
			Type::Bool => "boolean",
			Type::Nil => "nil",
			
			Type::Object(t) => t.type_name,
			Type::Iterator => "iterator",
		};
		write!(fmt, "{}", s)
	}
}

pub fn expected(exp: &str, got: &str) -> String {
	format!("Expected {}, got {}", exp, got)
}
