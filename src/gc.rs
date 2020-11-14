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

// Safety: .trace(ctx) must call .trace(ctx) on all direct GCRef or Trace children.
pub unsafe trait Trace: 'static {
	// Safety: must only be called on objects in the GC heap.
	unsafe fn trace(&self, _ctx: TraceCtx) {}
}

// Safety: Marker trait for types with no accessible GCRefs
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
unsafe impl<T: Trace> Trace for Vec<GCRef<T>> {
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
unsafe impl<T1: Trace, T2: Trace> Trace for HashMap<T1, GCRef<T2>> {
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

pub type GCCell<T> = GCRef<RefCell<T>>;


#[repr(C)]
pub(super) struct GCWrapper<T: Trace> {
	marked: Cell<bool>,
	root_cnt: Cell<u8>,
	data: T,
}

impl<T: Trace> GCWrapper<T> {
	fn new(value: T) -> GCWrapper<T> {
		GCWrapper {
			marked: Cell::new(false),
			root_cnt: Cell::new(0),
			data: value
		}
	}
}

impl<T: Trace> Deref for GCWrapper<T> {
	type Target = T;
	fn deref(&self) -> &T { &self.data }
}


trait GCWrapped {
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

impl<T: Trace> GCWrapped for GCWrapper<T> {
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


pub struct GCRef<T: Trace> {
	is_root: Cell<bool>,
	wrapper: *const GCWrapper<T>
}

impl<T: Trace> GCRef<T> {
	fn new(wrapper: &GCWrapper<T>) -> Self {
		(*wrapper).root();
		GCRef {
			is_root: Cell::new(true),
			wrapper,
		}
	}
	
	pub fn get_addr(&self) -> usize { self.wrapper as usize }
	
	// Safety: as long as the GC algorithm is correct, self.wrapper cannot be invalid,
	// except in the middle of the sweep step of GC, where this function must not be called.
	fn wrapper(&self) -> &GCWrapper<T> { unsafe { &*self.wrapper } }
	
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

impl<T: Trace> Clone for GCRef<T> {
	fn clone(&self) -> Self {
		GCRef::new(self.wrapper())
	}
}

impl<T: Trace> Drop for GCRef<T> {
	fn drop(&mut self) {
		unsafe { self.unroot(); }
	}
}

impl<T: Trace> Deref for GCRef<T> {
	type Target = T;
	
	fn deref(&self) -> &T {
		self.wrapper()
	}
}

impl<T: Trace + fmt::Debug> fmt::Debug for GCRef<T> {
	fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
		write!(fmt, "GCRef(")?;
		self.wrapper().fmt(fmt)?;
		write!(fmt, ")")
	}
}


const INIT_THRESHOLD: usize = 64;

pub struct GCHeap {
	objects: Vec<Box<dyn GCWrapped>>,
	threshold: usize,
	used: usize,
}

impl GCHeap {
	pub fn new() -> GCHeap {
		GCHeap {
			objects: vec![],
			threshold: INIT_THRESHOLD,
			used: 0,
		}
	}
	
	pub fn add<T: Trace>(&mut self, v: T) -> GCRef<T> {
		self.objects.push(Box::new(GCWrapper::new(v)));
		// Safety: we just pushed the object, so we know its concrete type
		let wrapper = unsafe { &*(&**self.objects.last().unwrap() as *const dyn GCWrapped as *const GCWrapper<T>) };
		self.used += wrapper.size();
		GCRef::new(wrapper)
	}
	
	pub fn add_cell<T: Trace>(&mut self, v: T) -> GCCell<T> {
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

// GCHeap panis when dropped if there are still living roots
impl Drop for GCHeap {
	fn drop(&mut self) {
		self.collect();
		if !self.is_empty() {
			self.inspect();
			panic!("GC heap was dropped with remaining references into it");
		}
	}
}
