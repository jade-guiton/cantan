use std::cell::{Cell, RefCell};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryFrom;
use std::fmt::{self, Write};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::rc::Rc;

use crate::chunk::CompiledFunction;
use crate::gc::{GcHeap, GcRef, GcCell, Trace, TraceCtx};
use crate::types::{DynType, StaticDynTyped, DynTyped};
use crate::value::Value;
use crate::vm::VmArena;
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
		format!("<{} 0x{:x}>", self.get_type().type_name, self.get_address())
	}
	fn index(&self, _idx: &Value) -> Option<Result<Value, String>> {
		None
	}
	fn prop(&self, _prop: &str, _gc: &mut GcHeap) -> Option<Value> {
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
	fn set_prop(&mut self, _prop: &str, _val: Value) -> Option<()> {
		None
	}
}

pub trait ImmObject: Object + AsObject {
	fn imm_hash(&self) -> u64 {
		self.get_address() as u64
	}
}

macro_rules! hashable_imm_type {
	($type:ty) => {
		impl ImmObject for $type {
			fn imm_hash(&self) -> u64 {
				let mut s = std::collections::hash_map::DefaultHasher::new();
				self.get_type().type_id.hash(&mut s);
				self.hash(&mut s);
				s.finish()
			}
		}
	};
}


impl dyn Object {
	pub fn is<T: Object>(&self) -> bool {
		self.as_any().is::<T>()
	}
	pub fn downcast<T: Object>(&self) -> Option<&T> {
		self.as_any().downcast_ref::<T>()
	}
}
impl GcRef<dyn ImmObject> {
	pub fn is<T: Object>(&self) -> bool {
		self.as_any().is::<T>()
	}
	pub fn downcast<T: ImmObject>(&self) -> Option<GcRef<T>> {
		if self.is::<T>() {
			unsafe { Some(self.cast::<T>()) }
		} else {
			None
		}
	}
}
impl GcCell<dyn MutObject> {
	pub fn is<T: Object>(&self) -> bool {
		self.borrow().as_any().is::<T>()
	}
	pub fn downcast<T: MutObject>(&self) -> Option<GcCell<T>> {
		if self.is::<T>() {
			unsafe { Some(self.cast::<RefCell<T>>()) }
		} else {
			None
		}
	}
}



#[derive(Trace, Hash)]
pub struct Tuple(pub Vec<Value>);
register_dyn_type!("tuple", Tuple);

impl Object for Tuple {
	fn struct_eq(&self, other: &dyn Object) -> bool {
		if let Some(other) = other.downcast::<Tuple>() {
			if self.0.len() == other.0.len() {
				self.0.iter().zip(other.0.deref()).all(|(a,b)| a.struct_eq(b))
			} else { false }
		} else { false }
	}
	fn cmp(&self, other: &dyn Object) -> Option<Ordering> {
		let other = other.downcast::<Tuple>()?;
		if self.0.len() != other.0.len() { return None; }
		let o = self.0.iter().zip(other.0.iter()).find_map(|(v1,v2)| {
			let res = v1.cmp(v2);
			if let Ok(Ordering::Equal) = res {
				None
			} else {
				Some(res)
			}
		});
		o.unwrap_or(Ok(Ordering::Equal)).ok()
	}
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
		Some({
			idx.get_int().and_then(|idx| {
				usize::try_from(idx).ok().and_then(|idx| self.0.get(idx)).cloned()
					.ok_or_else(|| format!("Trying to index {}-tuple with: {}", self.0.len(), idx))
			})
		})
	}
}
hashable_imm_type!(Tuple);


#[derive(Trace, PartialEq, Eq, Hash)]
pub struct List(pub Vec<Value>);
register_dyn_type!("list", List);

impl Object for List {
	fn struct_eq(&self, other: &dyn Object) -> bool {
		if let Some(other) = other.downcast::<List>() {
			if self.0.len() == other.0.len() {
				self.0.iter().zip(other.0.deref())
					.all(|(a,b)| a.struct_eq(b))
			} else { false }
		} else { false }
	}
	fn repr(&self) -> String {
		let mut buf = String::new();
		write!(buf, "[").unwrap();
		for (idx, val) in self.0.iter().enumerate() {
			write!(buf, "{}", val.repr()).unwrap();
			if idx < self.0.len() - 1 {
				write!(buf, ", ").unwrap();
			}
		}
		write!(buf, "]").unwrap();
		buf
	}
	fn index(&self, idx: &Value) -> Option<Result<Value, String>> {
		Some({
			idx.get_int().and_then(|idx| {
				usize::try_from(idx).ok().and_then(|idx| self.0.get(idx)).cloned()
					.ok_or_else(|| format!("Trying to index list of length {} with: {}", self.0.len(), idx))
			})
		})
	}
}
impl MutObject for List {
	fn set_index(&mut self, idx: Value, val: Value) -> Option<Result<(), String>> {
		Some({
			idx.get_int().and_then(|idx| {
				let len = self.0.len();
				let slot = usize::try_from(idx).ok().and_then(|idx| self.0.get_mut(idx))
					.ok_or_else(|| format!("Trying to index list of length {} with: {}", len, idx))?;
				*slot = val;
				Ok(())
			})
		})
	}
}


pub type Map = HashMap<Value, Value>;
register_dyn_type!("map", Map);

impl Object for Map {
	fn struct_eq(&self, other: &dyn Object) -> bool {
		if let Some(other) = other.downcast::<Map>() {
			if self.len() == other.len() {
				self.iter().all(|(k,v)| other.get(k).map_or(false, |v2| v.struct_eq(v2)))
			} else { false }
		} else { false }
	}
	fn repr(&self) -> String {
		let mut buf = String::new();
		write!(buf, "[").unwrap();
		if self.len() == 0 {
			write!(buf, "=").unwrap();
		} else {
			for (idx, (key, val)) in self.iter().enumerate() {
				write!(buf, "{}={}", key.repr(), val.repr()).unwrap();
				if idx < self.len() - 1 {
					write!(buf, ", ").unwrap();
				}
			}
		}
		write!(buf, "]").unwrap();
		buf
	}
	fn index(&self, idx: &Value) -> Option<Result<Value, String>> {
		Some({
			self.get(idx)
				.ok_or_else(||
					format!("Map does not contain key: {}", idx.repr()))
				.map(|v| v.clone())
		})
	}
}
impl MutObject for Map {
	fn set_index(&mut self, idx: Value, val: Value) -> Option<Result<(), String>> {
		self.insert(idx, val);
		Some(Ok(()))
	}
}


pub type Struct = HashMap<String, Value>;
register_dyn_type!("struct", Struct);

impl Object for Struct {
	fn struct_eq(&self, other: &dyn Object) -> bool {
		if let Some(other) = other.downcast::<Struct>() {
			if self.len() == other.len() {
				self.iter().all(|(k,v)| other.get(k).map_or(false, |v2| v.struct_eq(v2)))
			} else { false }
		} else { false }
	}
	fn repr(&self) -> String {
		let mut buf = String::new();
		write!(buf, "{{").unwrap();
		for (idx, (key, val)) in self.iter().enumerate() {
			write!(buf, "{}={}", key, val.repr()).unwrap();
			if idx < self.len() - 1 {
				write!(buf, ", ").unwrap();
			}
		}
		write!(buf, "}}").unwrap();
		buf
	}
	fn prop(&self, prop: &str, _gc: &mut GcHeap) -> Option<Value> {
		self.get(prop).cloned()
	}
}
impl MutObject for Struct {
	fn set_prop(&mut self, prop: &str, val: Value) -> Option<()> {
		self.get_mut(prop)
			.map(|slot| {
				*slot = val;
			})
	}
}


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
register_dyn_type!("function", Function);

impl Function {
	pub fn main(chunk: Rc<CompiledFunction>) -> Self {
		Function { chunk, upvalues: vec![] }
	}
}

impl Object for Function {}
impl ImmObject for Function {}


pub trait NativeFunction = Fn(&mut VmArena, Vec<Value>) -> Result<Value, String> + 'static;

pub struct NativeFunctionWrapper {
	pub func: Box<dyn NativeFunction>,
	pub this: Option<Value>,
}
register_dyn_type!("function", NativeFunctionWrapper);

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

impl Object for NativeFunctionWrapper {}
impl ImmObject for NativeFunctionWrapper {}


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
	Tuple(GcRef<Tuple>),
	List(GcCell<List>),
}

impl GcSequence {
	pub fn deref(&self) -> SmartRef<[Value]> {
		match self {
			GcSequence::Tuple(tuple) => SmartRef::Static(tuple.0.deref()),
			GcSequence::List(list) => SmartRef::Dynamic(std::cell::Ref::map(list.borrow(), |v| v.0.deref())),
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
