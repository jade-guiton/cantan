use std::hash::{Hash, Hasher};
use std::convert::TryFrom;
use std::collections::HashMap;
use std::cmp::Ordering;
use std::fmt::Write;
use std::ops::Deref;

use ordered_float::NotNan;

use crate::ast;
use crate::gc::{Trace, GcRef, GcCell, GcHeap};
use crate::types::*;
use crate::objects::*;


#[derive(Clone, Trace)]
pub enum Value {
	// Unpacked Primitive
	Nil,
	Bool(bool),
	Int(i32),
	Float(NotNan<f64>),
	String(Box<str>),
	
	ImmObject(GcRef<dyn ImmObject>),
	MutObject(GcCell<dyn MutObject>),
	
	List(GcCell<Vec<Value>>),
	Map(GcCell<HashMap<Value, Value>>),
	Struct(GcCell<HashMap<String, Value>>),
	Function(GcRef<Function>),
	NativeFunction(GcRef<NativeFunctionWrapper>),
	Iterator(GcCell<dyn NativeIterator>),
}

pub fn expected_types(exp: &str, got: &Value) -> String {
	expected(exp, &format!("{}", got.get_type()))
}

pub fn expected_type(exp: Type, got: &Value) -> String {
	expected_types(&format!("{}", exp), got)
}

macro_rules! get_prim {
	($name:ident, $rust_type:ty, $cn_type:ident) => {
		pub fn $name(&self) -> Result<$rust_type, String> {
			if let Value::$cn_type(v) = self {
				Ok(v.clone())
			} else {
				Err(expected_type(Type::$cn_type, self))
			}
		}
	};
}

impl Value {
	pub fn from_native_func(gc: &mut GcHeap, func: impl NativeFunction, this: Option<Value>) -> Self {
		Value::NativeFunction(gc.add(NativeFunctionWrapper::new(func, this)))
	}
	
	pub fn from_native_iter(gc: &mut GcHeap, iter: impl NativeIterator) -> Self {
		Value::Iterator(gc.add_cell(iter))
	}
	
	pub fn get_type(&self) -> Type {
		match self {
			Value::Nil => Type::Nil,
			Value::Bool(_) => Type::Bool,
			Value::Int(_) => Type::Int,
			Value::Float(_) => Type::Float,
			Value::String(_) => Type::String,
			
			Value::ImmObject(o) => Type::Object(o.get_type()),
			Value::MutObject(o) => Type::Object(o.borrow().get_type()),
			
			Value::List(_) => Type::List,
			Value::Map(_) => Type::Map,
			Value::Struct(_) => Type::Struct,
			Value::Function(_) => Type::Function,
			Value::NativeFunction(_) => Type::Function,
			Value::Iterator(_) => Type::Iterator,
		}
	}
	
	get_prim!(get_bool, bool, Bool);
	get_prim!(get_int, i32, Int);
	get_prim!(get_list, GcCell<Vec<Value>>, List);
	get_prim!(get_iter, GcCell<dyn NativeIterator>, Iterator);
	get_prim!(get_map, GcCell<HashMap<Value, Value>>, Map);
	
	pub fn get_sequence<'a>(&'a self) -> Result<GcSequence, String> {
		let opt = match self {
			Value::ImmObject(o) => {
				if let Some(tuple) = o.downcast::<Tuple>() {
					Some(GcSequence::Tuple(tuple))
				} else { None }
			},
			Value::MutObject(o) => {
				if let Some(list) = o.downcast::<List>() {
					Some(GcSequence::List(list))
				} else { None }
			},
			
			_ => None,
		};
		opt.ok_or_else(|| expected_types("sequence type", self))
	}
	
	pub fn get_string(&self) -> Result<String, String> {
		if let Value::String(s) = self {
			Ok(s.to_string())
		} else {
			Err(expected_type(Type::String, self))
		}
	}
	
	pub fn get_numeric(&self) -> Result<f64, String> {
		match self {
			Value::Int(i) => Ok(*i as f64),
			Value::Float(f) => Ok(**f),
			_ => {
				Err(expected_types("int or float", self))
			}
		}
	}
	
	pub fn get_callable(&self) -> Result<Callable, String> {
		let res = match self {
			Value::ImmObject(o) => o.get_callable(),
			
			Value::Function(f) => Some(Callable::Function(f.clone())),
			Value::NativeFunction(f) => Some(Callable::Native(f.clone())),
			_ => None,
		};
		res.ok_or_else(|| expected_type(Type::Function, self))
	}
	
	pub fn struct_eq(&self, other: &Value) -> bool {
		match (self, other) {
			(Value::Nil, Value::Nil) => true,
			(Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
			(Value::Int(i1), Value::Int(i2)) => i1 == i2,
			(Value::Int(i1), Value::Float(f2)) =>
				NotNan::new(*i1 as f64).unwrap() == *f2,
			(Value::Float(f1), Value::Int(i2)) =>
				*f1 == NotNan::new(*i2 as f64).unwrap(),
			(Value::Float(f1), Value::Float(f2)) => f1 == f2,
			(Value::String(s1), Value::String(s2)) => s1 == s2,
			
			(Value::ImmObject(o1), Value::ImmObject(o2)) => o1.struct_eq(o2.as_object()),
			(Value::MutObject(o1), Value::MutObject(o2)) => o1.borrow().struct_eq(o2.borrow().as_object()),
			
			(Value::List(l1), Value::List(l2)) => {
				let (l1, l2) = (l1.borrow(), l2.borrow());
				if l1.len() == l2.len() {
					l1.iter().zip(l2.deref()).all(|(a,b)| a.struct_eq(b))
				} else { false }
			},
			(Value::Map(m1), Value::Map(m2)) => { // Memory equality for keys, structural for values
				let (m1, m2) = (m1.borrow(), m2.borrow());
				if m1.len() == m2.len() {
					m1.iter().all(|(k,v)| m2.get(k).map_or(false, |v2| v.struct_eq(v2)))
				} else { false }
			},
			(Value::Struct(o1), Value::Struct(o2)) => {
				let (o1, o2) = (o1.borrow(), o2.borrow());
				if o1.len() == o2.len() {
					o1.iter().all(|(k,v)| o2.get(k).map_or(false, |v2| v.struct_eq(v2)))
				} else { false }
			},
			_ => false,
		}
	}
	
	pub fn cmp(&self, other: &Value) -> Result<Ordering, String> {
		let res = match (self, other) {
			(Value::Int(i1), Value::Int(i2)) =>
				Some(i1.cmp(i2)),
			(Value::Int(i1), Value::Float(f2)) =>
				Some(NotNan::new(*i1 as f64).unwrap().cmp(&f2)),
			(Value::Float(f1), Value::Int(i2)) =>
				Some(f1.cmp(&NotNan::new(*i2 as f64).unwrap())),
			(Value::Float(f1), Value::Float(f2)) =>
				Some(f1.cmp(&f2)),
			(Value::String(s1), Value::String(s2)) =>
				Some(s1.cmp(s2)),
			
			(Value::ImmObject(o1), Value::ImmObject(o2)) => o1.cmp(o2.as_object()),
			(Value::MutObject(o1), Value::MutObject(o2)) => o1.borrow().cmp(o2.borrow().as_object()),
			
			_ => None,
		};
		res.ok_or_else(|| format!("Cannot compare: '{}', '{}'", self.repr(), other.repr()))
	}
	
	pub fn repr(&self) -> String {
		match self {
			Value::Nil => String::from("nil"),
			Value::Bool(b) => format!("{:?}", b),
			Value::Int(i) => format!("{}", i),
			Value::Float(f) => {
				let mut s;
				if f.abs() >= 1e10 || f.abs() < 1e-2 {
					s = format!("{:e}", f.into_inner());
					if !s.contains(".") {
						let parts: Vec<&str> = s.split("e").collect();
						assert!(parts.len() == 2);
						s = format!("{}.0e{}", parts[0], parts[1]);
					}
				} else {
					s = format!("{}", f);
					if f.round() == f.into_inner() {
						s += ".0";
					}
				}
				s
			}
			Value::String(s) => format!("{:?}", s),
			
			Value::ImmObject(o) => o.repr(),
			Value::MutObject(o) => o.borrow().repr(),
			
			Value::List(list) => {
				let list = list.borrow();
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
			Value::Map(map) => {
				let map = map.borrow();
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
			Value::Struct(obj) => {
				let obj = obj.borrow();
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
			Value::Function(func) =>
				format!("<fn 0x{:x}>", func.get_addr() as usize),
			Value::NativeFunction(func) =>
				format!("<fn 0x{:x}>", func.get_addr() as usize),
			Value::Iterator(iter) =>
				format!("<iter 0x{:x}>", iter.get_addr() as usize),
		}
	}
	
	pub fn index(&self, idx: &Value) -> Result<Value, String> {
		let res = match self {
			Value::ImmObject(o) => o.index(idx),
			Value::MutObject(o) => o.borrow().index(idx),
			
			Value::List(list) => {
				let list = list.borrow();
				let idx = idx.get_int()?;
				Some(usize::try_from(idx).ok().and_then(|idx| list.get(idx)).cloned()
					.ok_or_else(|| format!("Trying to index list of length {} with: {}", list.len(), idx)))
			},
			Value::Map(map) => {
				Some(Ok(map.borrow().get(idx).ok_or_else(|| format!("Map does not contain key: {}", idx.repr()))?.clone()))
			},
			_ => None,
		};
		res.ok_or_else(|| format!("Cannot get index from {}", self.get_type())).flatten()
	}
	
	pub fn set_index(&self, idx: Value, val: Value) -> Result<(), String> {
		let res = match self {
			Value::MutObject(o) => {
				if let Some(res) = o.borrow_mut().set_index(idx, val) {
					Some(res?)
				} else {
					None
				}
			},
			
			Value::List(list) => {
				let mut list = list.borrow_mut();
				let idx = idx.get_int()?;
				let len = list.len();
				let slot = usize::try_from(idx).ok().and_then(|idx| list.get_mut(idx))
					.ok_or_else(|| format!("Trying to index list of length {} with: {}", len, idx))?;
				*slot = val;
				Some(())
			},
			Value::Map(map) => {
				let mut map = map.borrow_mut();
				map.insert(idx, val);
				Some(())
			},
			_ => None,
		};
		res.ok_or_else(|| format!("Cannot set index in {}", self.get_type()))
	}
	
	pub fn prop(&self, prop: &str, gc: &mut GcHeap) -> Result<Value, String> {
		let res = match self {
			Value::MutObject(o) => o.borrow().prop(prop, gc),
			
			Value::Struct(obj) => {
				Some(obj.borrow().get(prop).ok_or_else(|| format!("Object does not have prop '{}'", prop))?.clone())
			},
			_ => None,
		};
		let res = res.or_else(|| {
			crate::stdlib::METHODS.get(&self.get_type())
				.and_then(|mets| mets.get(prop))
				.map(|met| Value::from_native_func(gc, met, Some(self.clone())))
		});
		res.ok_or_else(|| format!("Cannot get prop '{}' of {}", prop, self.get_type()))
	}
	
	pub fn set_prop(&self, prop: &str, val: Value) -> Result<(), String> {
		let res = match self {
			Value::MutObject(o) => {
				if let Some(res) = o.borrow_mut().set_prop(prop, val) {
					Some(res?)
				} else {
					None
				}
			},
			
			Value::Struct(obj) => {
				let mut obj = obj.borrow_mut();
				let slot = obj.get_mut(prop).ok_or_else(|| format!("Object does not have prop '{}'", prop))?;
				*slot = val;
				Some(())
			},
			_ => None,
		};
		res.ok_or_else(|| format!("Cannot get prop of {}", self.get_type()))
	}
	
	pub fn make_iter(&self, gc: &mut GcHeap) -> Result<Option<Value>, String> {
		if let Value::Iterator(_) = self {
			return Ok(None);
		}
		let seq = self.get_sequence()
			.map_err(|_| format!("Cannot iterate on {}", self.repr()))?;
		Ok(Some(Value::from_native_iter(gc, SequenceIterator::new(seq))))
	}
	
	pub fn is<T: Object>(&self) -> bool {
		match self {
			Value::ImmObject(o) => o.is::<T>(),
			Value::MutObject(o) => o.is::<T>(),
			_ => false,
		}
	}
	pub fn get_imm<T: ImmObject + StaticDynTyped>(&self) -> Result<GcRef<T>, String> {
		let res = if let Value::ImmObject(o) = self {
			o.downcast::<T>()
		} else {
			None
		};
		res.ok_or_else(|| expected_types(<T as StaticDynTyped>::DYN_TYPE.type_name, self))
	}
	pub fn get_mut<T: MutObject + StaticDynTyped>(&self) -> Result<GcCell<T>, String> {
		let res = if let Value::MutObject(o) = self {
			o.downcast::<T>()
		} else {
			None
		};
		res.ok_or_else(|| expected_types(<T as StaticDynTyped>::DYN_TYPE.type_name, self))
	}
}

// Note: This is memory equality, not structural identity;
// The "==" operator in-language uses struct_eq() instead.
// This is essentially used in the implementation of maps.
impl PartialEq for Value {
	fn eq(&self, other: &Value) -> bool {
		match (self, other) {
			(Value::Nil, Value::Nil) => true,
			(Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
			(Value::Int(i1), Value::Int(i2)) => i1 == i2,
			(Value::Float(f1), Value::Float(f2)) => f1 == f2,
			(Value::String(s1), Value::String(s2)) => s1 == s2,
			
			(Value::ImmObject(o1), Value::ImmObject(o2)) => o1.struct_eq(o2.as_object()),
			(Value::MutObject(o1), Value::MutObject(o2)) => o1.get_addr() == o2.get_addr(),
			
			(Value::List(l1), Value::List(l2)) => l1.get_addr() == l2.get_addr(),
			(Value::Map(m1), Value::Map(m2)) => m1.get_addr() == m2.get_addr(),
			(Value::Struct(o1), Value::Struct(o2)) => o1.get_addr() == o2.get_addr(),
			(Value::Function(f1), Value::Function(f2)) => f1.get_addr() == f2.get_addr(),
			(Value::NativeFunction(f1), Value::NativeFunction(f2)) => f1.get_addr() == f2.get_addr(),
			(Value::Iterator(i1), Value::Iterator(i2)) => i1.get_addr() == i2.get_addr(),
			_ => false,
		}
	}
}
impl Eq for Value {}

impl Hash for Value {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.get_type().hash(state);
		match self {
			Value::Nil => {},
			Value::Bool(b) => b.hash(state),
			Value::Int(i) => i.hash(state),
			Value::Float(f) => f.hash(state),
			Value::String(s) => s.hash(state),
			
			Value::ImmObject(o) => o.imm_hash().hash(state),
			Value::MutObject(o) => o.get_addr().hash(state),
			
			Value::List(list) => list.get_addr().hash(state),
			Value::Map(map) => map.get_addr().hash(state),
			Value::Struct(obj) => obj.get_addr().hash(state),
			Value::Function(func) => func.get_addr().hash(state),
			Value::NativeFunction(func) => func.get_addr().hash(state),
			Value::Iterator(iter) => iter.get_addr().hash(state),
		}
	}
}

impl From<&ast::Primitive> for Value {
	fn from(cst: &ast::Primitive) -> Self {
		match cst {
			ast::Primitive::Nil => Value::Nil,
			ast::Primitive::Bool(b) => Value::Bool(*b),
			ast::Primitive::Int(i) => Value::Int(*i),
			ast::Primitive::Float(f) => Value::Float(*f),
			ast::Primitive::String(s) => Value::String(s.clone()),
		}
	}
}

impl From<String> for Value {
	fn from(s: String) -> Self {
		Value::String(s.into_boxed_str())
	}
}

impl TryFrom<f64> for Value {
	type Error = String;
	fn try_from(f: f64) -> Result<Self, String> {
		if f.is_finite() {
			Ok(Value::Float(NotNan::new(f).unwrap()))
		} else {
			Err(String::from("Float operation had Infinite or NaN result"))
		}
	}
}
