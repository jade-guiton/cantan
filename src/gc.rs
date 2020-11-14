use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::mem::size_of;
use std::ops::Deref;

#[derive(PartialEq, Eq, Clone, Copy)]
enum TraceAction {
	Unroot,
	Mark,
}
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct TraceCtx(TraceAction);

// Safety: .trace(ctx) must call .trace(ctx) on all direct GcRef or Trace children.
pub unsafe trait Trace: 'static {
	// Safety: must only be called on objects in the GC heap.
	unsafe fn trace(&self, _ctx: TraceCtx) {}
}

// Safety: Marker trait for types with no accessible GcRefs
pub unsafe trait Primitive: 'static {}
unsafe impl<T: Primitive> Trace for T {}

unsafe impl Primitive for () {}
unsafe impl Primitive for bool {}
unsafe impl Primitive for i32 {}
unsafe impl Primitive for usize {}
unsafe impl Primitive for String {}
unsafe impl<T: Primitive> Primitive for Cell<T> {}

unsafe impl<T: Trace> Trace for Option<T> {
	unsafe fn trace(&self, ctx: TraceCtx) {
		if let Some(x) = self {
			x.trace(ctx);
		}
	}
}

unsafe impl<T: Trace> Trace for Vec<T> {
	unsafe fn trace(&self, ctx: TraceCtx) {
		for x in self {
			x.trace(ctx);
		}
	}
}
unsafe impl<T: Trace> Trace for Vec<GcRef<T>> {
	unsafe fn trace(&self, ctx: TraceCtx) {
		for x in self {
			x.trace(ctx);
		}
	}
}

unsafe impl<T1: Trace, T2: Trace> Trace for HashMap<T1, T2> {
	unsafe fn trace(&self, ctx: TraceCtx) {
		for x in self.keys() {
			x.trace(ctx);
		}
		for x in self.values() {
			x.trace(ctx);
		}
	}
}
unsafe impl<T1: Trace, T2: Trace> Trace for HashMap<T1, GcRef<T2>> {
	unsafe fn trace(&self, ctx: TraceCtx) {
		for x in self.keys() {
			x.trace(ctx);
		}
		for x in self.values() {
			x.trace(ctx);
		}
	}
}

unsafe impl<T: Trace> Trace for RefCell<T> {
	unsafe fn trace(&self, ctx: TraceCtx) {
		self.borrow().trace(ctx)
	}
}

pub type GcCell<T> = GcRef<RefCell<T>>;


#[repr(C)]
pub(super) struct GcWrapper<T: Trace> {
	marked: Cell<bool>,
	root_cnt: Cell<u8>,
	data: T,
}

impl<T: Trace> GcWrapper<T> {
	fn new(value: T) -> GcWrapper<T> {
		GcWrapper {
			marked: Cell::new(false),
			root_cnt: Cell::new(0),
			data: value
		}
	}
}

impl<T: Trace> Deref for GcWrapper<T> {
	type Target = T;
	fn deref(&self) -> &T { &self.data }
}


trait GcWrapped {
	fn root(&self);
	fn unroot(&self);
	fn is_rooted(&self) -> bool;
	fn root_cnt(&self) -> u8;
	fn unroot_children(&self);
	fn mark(&self);
	fn unmark(&self);
	fn is_marked(&self) -> bool;
	fn size(&self) -> usize;
}

impl<T: Trace> GcWrapped for GcWrapper<T> {
	fn root(&self) {
		self.root_cnt.set(self.root_cnt.get() + 1);
	}
	fn unroot(&self) {
		self.root_cnt.set(self.root_cnt.get() - 1);
	}
	fn is_rooted(&self) -> bool { self.root_cnt.get() > 0 }
	fn root_cnt(&self) -> u8 { self.root_cnt.get() }
	fn unroot_children(&self) {
		unsafe { self.data.trace(TraceCtx(TraceAction::Unroot)); }
	}
	
	fn mark(&self) {
		if !self.marked.get() {
			self.marked.set(true);
			unsafe { self.data.trace(TraceCtx(TraceAction::Mark)); }
		}
	}
	fn unmark(&self) {
		self.marked.set(false);
	}
	fn is_marked(&self) -> bool { self.marked.get() }
	
	fn size(&self) -> usize {
		size_of::<Self>()
	}
}


pub struct GcRef<T: Trace> {
	is_root: Cell<bool>,
	wrapper: *const GcWrapper<T>
}

impl<T: Trace> GcRef<T> {
	fn new(wrapper: &GcWrapper<T>) -> Self {
		(*wrapper).root();
		GcRef {
			is_root: Cell::new(true),
			wrapper,
		}
	}
	
	pub fn get_addr(&self) -> usize { self.wrapper as usize }
	
	// Safety: as long as the GC algorithm is correct, self.wrapper cannot be invalid,
	// except in the middle of the sweep step of GC, where this function must not be called.
	fn wrapper(&self) -> &GcWrapper<T> { unsafe { &*self.wrapper } }
	
	// Safety: reference must be on the GC heap or about to be dropped
	unsafe fn unroot(&self) {
		if self.is_root.get() {
			self.wrapper().unroot();
			self.is_root.set(false);
		}
	}
	
	// Safety: reference must be on the GC heap
	pub unsafe fn trace(&self, ctx: TraceCtx) {
		match ctx.0 {
			TraceAction::Unroot => self.unroot(),
			TraceAction::Mark => self.wrapper().mark(),
		}
	}
}

impl<T: Trace> Clone for GcRef<T> {
	fn clone(&self) -> Self {
		GcRef::new(self.wrapper())
	}
}

impl<T: Trace> Drop for GcRef<T> {
	fn drop(&mut self) {
		unsafe { self.unroot(); }
	}
}

impl<T: Trace> Deref for GcRef<T> {
	type Target = T;
	
	fn deref(&self) -> &T {
		self.wrapper()
	}
}

impl<T: Trace + fmt::Debug> fmt::Debug for GcRef<T> {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		write!(fmt, "GcRef(")?;
		self.wrapper().fmt(fmt)?;
		write!(fmt, ")")
	}
}


const INIT_THRESHOLD: usize = 64;

pub struct GcHeap {
	objects: Vec<Box<dyn GcWrapped>>,
	threshold: usize,
	used: usize,
}

impl GcHeap {
	pub fn new() -> GcHeap {
		GcHeap {
			objects: vec![],
			threshold: INIT_THRESHOLD,
			used: 0,
		}
	}
	
	pub fn add<T: Trace>(&mut self, v: T) -> GcRef<T> {
		self.objects.push(Box::new(GcWrapper::new(v)));
		// Safety: we just pushed the object, so we know its concrete type
		let wrapper = unsafe { &*(&**self.objects.last().unwrap() as *const dyn GcWrapped as *const GcWrapper<T>) };
		self.used += wrapper.size();
		GcRef::new(wrapper)
	}
	
	pub fn add_cell<T: Trace>(&mut self, v: T) -> GcCell<T> {
		self.add(RefCell::new(v))
	}
	
	pub fn collect(&mut self) {
		for wrapper in self.objects.iter() {
			wrapper.unroot_children();
		}

		for wrapper in self.objects.iter() {
			if wrapper.is_rooted() {
				wrapper.mark();
			}
		}
		
		self.objects.retain(|wrapper| wrapper.is_marked());
		
		self.used = 0;
		for wrapper in self.objects.iter_mut() {
			wrapper.unmark();
			self.used += wrapper.size();
		}
	}
	
	pub fn step(&mut self) {
		if self.used >= self.threshold {
			self.collect();
			self.threshold = self.used * 2;
		}
	}
	
	pub fn is_empty(&self) -> bool {
		self.objects.is_empty()
	}
	
	pub fn inspect(&self) {
		let mut roots = 0;
		for wrapper in self.objects.iter() {
			roots += wrapper.root_cnt() as usize;
		}
		println!("GC Heap has {} objects, taking up {} bytes, referenced by {} roots, ",
			self.objects.len(), self.used, roots);
	}
}

// GcHeap panics when dropped if there are still living roots
impl Drop for GcHeap {
	fn drop(&mut self) {
		self.collect();
		if !self.is_empty() {
			self.inspect();
			panic!("GC heap was dropped with remaining references into it");
		}
	}
}
