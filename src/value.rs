use std::hash::{Hash, Hasher};
use std::cell::Cell;
use std::convert::TryFrom;
use std::collections::HashMap;
use std::cmp::Ordering;
use std::fmt::{self, Write};
use std::ops::Deref;
use std::rc::Rc;

use ordered_float::NotNan;

use crate::ast;
use crate::chunk::CompiledFunction;
use crate::gc::{Trace, TraceCtx, GcRef, GcCell, GcHeap};
use crate::vm::VmArena;
use crate::types::*;
use crate::register_dyn_type;


pub trait Object: Trace + DynTyped {
	fn get_address(&self) -> usize {
		self as *const Self as *const () as usize
	}
	fn get_sequence<'a>(&'a self) -> Option<SmartRef<'a, [Value]>> {
		None
	}
	fn get_callable(&self) -> Option<Callable> {
		None
	}
	fn struct_eq(&self, other: &dyn Object) -> bool {
		self.get_address() == other.get_address()
	}
	fn cmp(&self, _other: &dyn Object) -> Option<Ordering> {
		None
	}
	fn repr(&self) -> String {
		format!("{}(0x{:x})", self.get_type().type_name, self.get_address())
	}
	fn index(&self, _idx: &Value) -> Option<Result<Value, String>> {
		None
	}
	fn prop(&self, _prop: &str, _gc: &mut GcHeap) -> Option<Value> {
		None
	}
	fn make_iter(&self, _gc: &mut GcHeap) -> Option<Option<Value>> {
		None
	}
}

pub trait AsObject {
	fn as_object(&self) -> &dyn Object;
}
impl<T: Object> AsObject for T {
	fn as_object(&self) -> &dyn Object {
		self
	}
}

pub trait MutObject: Object + AsObject {
	fn set_index(&mut self, _idx: Value, _val: Value) -> Option<Result<(), String>> {
		None
	}
	fn set_prop(&mut self, _prop: &str, _val: Value) -> Option<Result<(), String>> {
		None
	}
}

pub trait ImmObject: Object + AsObject + ImmData {}

#[derive(Trace, PartialEq, Eq, Hash)]
pub struct Tuple(pub Vec<Value>);
register_dyn_type!("tuple", Tuple);

impl Object for Tuple {
	fn repr(&self) -> String {
		let mut buf = String::new();
		write!(buf, "(").unwrap();
		for (idx, val) in self.0.iter().enumerate() {
			write!(buf, "{}", val.repr()).unwrap();
			if idx < self.0.len() - 1 {
				write!(buf, ", ").unwrap();
			}
		}
		if self.0.len() == 1 {
			write!(buf, ",").unwrap();
		}
		write!(buf, ")").unwrap();
		buf
	}
	
	fn index(&self, idx: &Value) -> Option<Result<Value, String>> {
		let idx = match idx.get_int() {
			Ok(idx) => idx,
			Err(err) => return Some(Err(err)),
		};
		Some(usize::try_from(idx).ok().and_then(|idx| self.0.get(idx)).cloned()
			.ok_or_else(|| format!("Trying to index {}-tuple with: {}", self.0.len(), idx)))
	}
}
impl ImmObject for Tuple {}


#[derive(Trace, PartialEq, Eq, Hash)]
pub struct List(pub Vec<Value>);
register_dyn_type!("list", List);

impl Object for List {}
impl MutObject for List {}


#[derive(Trace)]
pub enum Upvalue {
	Open(usize),
	Closed(Value),
}

#[derive(Trace)]
pub struct Function {
	pub chunk: Rc<CompiledFunction>,
	pub upvalues: Vec<GcCell<Upvalue>>,
}

impl Function {
	pub fn main(chunk: Rc<CompiledFunction>) -> Self {
		Function { chunk, upvalues: vec![] }
	}
}

pub trait NativeFunction = Fn(&mut VmArena, Vec<Value>) -> Result<Value, String> + 'static;

pub struct NativeFunctionWrapper {
	pub func: Box<dyn NativeFunction>,
	pub this: Option<Value>,
}

impl NativeFunctionWrapper {
	pub fn new(func: impl NativeFunction, this: Option<Value>) -> Self {
		NativeFunctionWrapper { func: Box::new(func), this }
	}
	
	pub fn call(&self, vm: &mut VmArena, mut args: Vec<Value>) -> Result<Value, String> {
		if let Some(this) = &self.this {
			args.insert(0, this.clone());
		}
		(self.func)(vm, args)
	}
}

impl fmt::Debug for NativeFunctionWrapper {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		write!(fmt, "NativeFunctionWrapper @ 0x{:x}", self.func.as_ref() as *const _ as *const () as usize)
	}
}

// Not sure if this is safe?
// I don't think a NativeFunction can "contain" a GcRef, since it is required
// to last for a 'static lifetime, but I'm not 100% sure.
unsafe impl Trace for NativeFunctionWrapper {
	unsafe fn trace(&self, ctx: TraceCtx) {
		if let Some(this) = &self.this {
			this.trace(ctx);
		}
	}
}

pub trait NativeIterator: Trace {
	fn next(&mut self, vm: &mut VmArena) -> Result<Option<Value>, String>;
}

pub enum SmartRef<'a, T: ?Sized> {
	Static(&'a T),
	Dynamic(std::cell::Ref<'a, T>),
}

impl<'a, T: ?Sized> Deref for SmartRef<'a, T> {
	type Target = T;
	fn deref(&self) -> &T {
		match self {
			SmartRef::Static(r) => r,
			SmartRef::Dynamic(r) => r.deref(),
		}
	}
}

#[derive(Trace)]
pub enum GcSequence {
	Tuple(GcRef<Vec<Value>>),
	List(GcCell<Vec<Value>>),
}

impl GcSequence {
	fn deref(&self) -> SmartRef<[Value]> {
		match self {
			GcSequence::Tuple(tuple) => SmartRef::Static(tuple.deref()),
			GcSequence::List(list) => SmartRef::Dynamic(std::cell::Ref::map(list.borrow(), |v| v.deref())),
		}
	}
}

#[derive(Trace)]
pub struct SequenceIterator {
	seq: GcSequence,
	idx: Cell<usize>,
}

impl SequenceIterator {
	pub fn new(seq: GcSequence) -> Self {
		SequenceIterator {
			seq,
			idx: Cell::new(0),
		}
	}
}

impl NativeIterator for SequenceIterator {
	fn next(&mut self, _vm: &mut VmArena) -> Result<Option<Value>, String> {
		if let Some(val) = self.seq.deref().get(self.idx.get()).cloned() {
			self.idx.set(self.idx.get() + 1);
			Ok(Some(val))
		} else {
			Ok(None)
		}
	}
}

#[derive(Trace)]
pub enum Callable {
	Function(GcRef<Function>),
	Native(GcRef<NativeFunctionWrapper>),
}

impl Callable {
	pub fn call(&self, vm: &mut VmArena, args: Vec<Value>) -> Result<Value, String> {
		match self {
			Callable::Function(func) => vm.call_function(func.clone(), args),
			Callable::Native(func) => func.call(vm, args),
		}
	}
}

#[derive(Clone, Trace)]
pub enum Value {
	// Unpacked Primitive
	Nil,
	Bool(bool),
	Int(i32),
	Float(NotNan<f64>),
	String(Box<str>),
	
	ImmObject(GcRef<dyn ImmObject>),
	Object(GcCell<dyn MutObject>),
	
	Tuple(GcRef<Vec<Value>>),
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
			Value::Object(o) => Type::Object(o.borrow().get_type()),
			
			Value::Tuple(_) => Type::Tuple,
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
	get_prim!(get_tuple, GcRef<Vec<Value>>, Tuple);
	get_prim!(get_iter, GcCell<dyn NativeIterator>, Iterator);
	get_prim!(get_map, GcCell<HashMap<Value, Value>>, Map);
	
	pub fn get_sequence<'a>(&'a self) -> Result<SmartRef<'a,[Value]>, String> {
		let opt: Option<SmartRef<'a,[Value]>> = match self {
			Value::ImmObject(o) => {
				o.as_any().downcast_ref::<Tuple>().map(|tuple|
					SmartRef::Static(tuple.0.deref()))
			},
			Value::Object(o) => {
				if std::cell::Ref::map(o.borrow(), |o2| o2.as_any()).is::<List>() {
					Some(SmartRef::Dynamic(std::cell::Ref::map(o.borrow(), |o2|
						o2.as_any().downcast_ref::<List>().unwrap().0.deref())))
				} else {
					None
				}
			}
			
			Value::Tuple(tuple) => Some(SmartRef::Static(tuple)),
			Value::List(list) => Some(SmartRef::Dynamic(std::cell::Ref::map(list.borrow(), |l| l.deref()))),
			
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
			(Value::Object(o1), Value::Object(o2)) => o1.borrow().struct_eq(o2.borrow().as_object()),
			
			(Value::Tuple(t1), Value::Tuple(t2)) =>
				if t1.len() == t2.len() {
					t1.iter().zip(t2.deref()).all(|(a,b)| a.struct_eq(b))
				} else { false },
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
			(Value::Object(o1), Value::Object(o2)) => o1.borrow().cmp(o2.borrow().as_object()),
			
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
					Some(o.unwrap_or(Ok(Ordering::Equal))?)
				} else {
					None
				}
			}
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
			Value::Object(o) => o.borrow().repr(),
			
			Value::Tuple(list) => {
				let mut buf = String::new();
				write!(buf, "(").unwrap();
				for (idx, val) in list.iter().enumerate() {
					write!(buf, "{}", val.repr()).unwrap();
					if idx < list.len() - 1 {
						write!(buf, ", ").unwrap();
					}
				}
				if list.len() == 1 {
					write!(buf, ",").unwrap();
				}
				write!(buf, ")").unwrap();
				buf
			},
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
			Value::Object(o) => o.borrow().index(idx),
			
			Value::Tuple(tuple) => {
				let idx = idx.get_int()?;
				Some(usize::try_from(idx).ok().and_then(|idx| tuple.get(idx)).cloned()
					.ok_or_else(|| format!("Trying to index {}-tuple with: {}", tuple.len(), idx)))
			},
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
			Value::Object(o) => {
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
			Value::ImmObject(o) => o.prop(prop, gc),
			Value::Object(o) => o.borrow().prop(prop, gc),
			
			Value::Struct(obj) => {
				Some(obj.borrow().get(prop).ok_or_else(|| format!("Object does not have prop '{}'", prop))?.clone())
			},
			_ => {
				if let Some(met) = crate::stdlib::METHODS.get(&self.get_type()).and_then(|mets| mets.get(prop)) {
					Some(Value::from_native_func(gc, met, Some(self.clone())))
				} else {
					None
				}
			}
		};
		res.ok_or_else(|| format!("Cannot get prop '{}' of {}", prop, self.get_type()))
	}
	
	pub fn set_prop(&self, prop: &str, val: Value) -> Result<(), String> {
		let res = match self {
			Value::Object(o) => {
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
		let res = match self {
			Value::ImmObject(o) => o.make_iter(gc),
			Value::Object(o) => o.borrow().make_iter(gc),
			
			Value::List(list) => {
				Some(Some(Value::from_native_iter(gc, SequenceIterator::new(GcSequence::List(list.clone())))))
			},
			Value::Tuple(tuple) => {
				Some(Some(Value::from_native_iter(gc, SequenceIterator::new(GcSequence::Tuple(tuple.clone())))))
			},
			Value::Iterator(_) => Some(None),
			_ => None,
		};
		res.ok_or_else(|| format!("Cannot iterate: {}", self.repr()))
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
			(Value::Object(o1), Value::Object(o2)) => o1.get_addr() == o2.get_addr(),
			
			(Value::Tuple(t1), Value::Tuple(t2)) => **t1 == **t2,
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
			Value::Object(o) => o.get_addr().hash(state),
			
			Value::Tuple(values) => values.hash(state),
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
