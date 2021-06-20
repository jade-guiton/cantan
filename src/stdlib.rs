use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::iter::Iterator;
use std::ops::Deref;

use rand::prelude::random;
use once_cell::sync::Lazy;
use unicode_segmentation::{UnicodeSegmentation, GraphemeCursor};

use crate::types::{Type, StaticDynTyped, expected};
use crate::objects::{NativeIterator, Callable, Tuple, List, Map};
use crate::value::Value;
use crate::gc::{Trace, Primitive, GcCell, GcHeap};
use crate::vm::VmArena;

// To avoid writing the same type signature every time
macro_rules! native_func {
	($id:ident, $vm:ident, $args:ident, $block:block) => {
		fn $id($vm: &mut VmArena, $args: Vec<Value>) -> Result<Value, String> {
			$block
		}
	};
	($id:ident, $args:ident, $block:block) => { native_func!($id, _vm, $args, $block); };
	($id:ident, $block:block) => { native_func!($id, _vm, _args, $block); };
}

fn check_arg_cnt(exp: usize, cnt: usize) -> Result<(), String> {
	if cnt != exp {
		Err(expected(&format!("{} arguments", exp), &cnt.to_string()))
	} else {
		Ok(())
	}
}

native_func!(log, args, {
	for (i, arg) in args.iter().enumerate() {
		print!("{}", arg.repr());
		if i != args.len() - 1 {
			print!(" ");
		}
	}
	println!();
	Ok(Value::Nil)
});

native_func!(writeln, args, {
	check_arg_cnt(1, args.len())?;
	let val = args[0].get_string()?;
	println!("{}", val);
	Ok(Value::Nil)
});

native_func!(repr, args, {
	check_arg_cnt(1, args.len())?;
	Ok(Value::from(args[0].repr()))
});


struct IntIterator {
	next: Cell<i32>,
	until: i32,
	step: i32,
}
unsafe impl Primitive for IntIterator {}

impl NativeIterator for IntIterator {
	fn next(&mut self, _vm: &mut VmArena) -> Result<Option<Value>, String> {
		let next = self.next.get();
		if next < self.until {
			self.next.set(next + self.step);
			Ok(Some(Value::Int(next)))
		} else {
			Ok(None)
		}
	}
}

fn check_range_args(args: &[Value]) -> Result<(Option<i32>, i32, i32), String> {
	if args.is_empty() || args.len() > 3 {
		return Err(expected("1-3 arguments", &args.len().to_string()));
	}
	let (start, until) = if args.len() == 1 {
		(None, args[0].get_int()?)
	} else {
		(Some(args[0].get_int()?), args[1].get_int()?)
	};
	let step = if args.len() == 3 { args[2].get_int()? } else { 1 };
	Ok((start, until, step))
}

native_func!(xrange, vm, args, {
	let (start, until, step) = check_range_args(&args)?;
	Ok(Value::from_native_iter(&mut vm.gc, IntIterator {
		next: Cell::new(start.unwrap_or(0)),
		until,
		step
	}))
});

native_func!(range, vm, args, {
	let (start, until, step) = check_range_args(&args)?;
	Ok(Value::from_native_iter(&mut vm.gc, IntIterator {
		next: Cell::new(start.unwrap_or(1)),
		until: until + 1,
		step
	}))
});

native_func!(rand_range, args, {
	let (start, until, step) = check_range_args(&args)?;
	let start = start.unwrap_or(1);
	let n = (until+1-start) / step;
	Ok(Value::Int(start + random::<i32>().rem_euclid(n) * step))
});

native_func!(type_, args, {
	check_arg_cnt(1, args.len())?;
	Ok(Value::String(format!("{}", args[0].get_type()).into_boxed_str()))
});

native_func!(read_file, args, {
	check_arg_cnt(1, args.len())?;
	let path = args[0].get_string()?;
	let contents = std::fs::read_to_string(path).map_err(|e| format!("IO Error: {}", e))?;
	Ok(Value::from(contents))
});

native_func!(error, args, {
	let mut msg = String::from("User error");
	if !args.is_empty() {
		msg += ": ";
	}
	for (i, arg) in args.iter().enumerate() {
		if let Ok(s) = arg.get_string() {
			msg += &s;
		} else {
			msg += &arg.repr();
		}
		if i != args.len()-1 {
			msg += " ";
		}
	}
	Err(msg)
});

native_func!(seq_size, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let seq = args[0].get_sequence()?;
	let seq = seq.deref();
	let len = i32::try_from(seq.len())
		.map_err(|_| String::from("Sequence length does not fit in integer"))?;
	Ok(Value::Int(len))
});

fn wrap_index(idx: i32, len: usize) -> Option<usize> {
	if idx >= 0 {
		Some(idx as usize)
	} else if -idx as usize <= len {
		Some(len - (-idx as usize))
	} else {
		None
	}
}

native_func!(seq_sub, vm, args, {
	if args.len() < 2 || args.len() > 3 {
		return Err(expected("1/2 arguments", &(args.len() - 1).to_string()));
	}
	let seq = args[0].get_sequence()?;
	let seq = seq.deref();
	let start = args[1].get_int()?;
	let start2 = wrap_index(start, seq.len());
	let end = args.get(2).map(|v| v.get_int()).transpose()?;
	let end2 = end.map(|idx| wrap_index(idx, seq.len()))
		.unwrap_or_else(|| Some(seq.len()));
	let sub = start2.and_then(|start| end2.and_then(|end| seq.get(start..end)))
		.ok_or_else(||
			format!("Cannot take sublist {}..{} of sequence of length {}",
				start, end.map(|i| i.to_string()).unwrap_or_else(String::new), seq.len())
		)?.to_vec();
	if args[0].is::<Tuple>() {
		Ok(vm.gc.add_imm(Tuple(sub)))
	} else if args[0].is::<List>() {
		Ok(vm.gc.add_mut(List(sub)))
	} else {
		unimplemented!()
	}
});

native_func!(seq_to_iter, vm, args, {
	check_arg_cnt(0, args.len() - 1)?;
	args[0].make_iter(&mut vm.gc).transpose().unwrap()
});

native_func!(seq_map, vm, args, {
	if args.len() != 2 {
		return Err(expected("1 argument", &(args.len() - 1).to_string()));
	}
	let seq = args[0].get_sequence()?;
	let seq = seq.deref();

	let mut mapped = vec![];
	let callable = args[1].get_callable()?;
	for x in seq.deref() {
		mapped.push(callable.call(vm, vec![x.clone()])?);
	}
	
	if args[0].is::<Tuple>() {
		Ok(vm.gc.add_imm(Tuple(mapped)))
	} else if args[0].is::<List>() {
		Ok(vm.gc.add_mut(List(mapped)))
	} else {
		unimplemented!()
	}
});

native_func!(seq_filter, vm, args, {
	if args.len() != 2 {
		return Err(expected("1 argument", &(args.len() - 1).to_string()));
	}
	let seq = args[0].get_sequence()?;
	let seq = seq.deref();

	let mut filtered = vec![];
	let callable = args[1].get_callable()?;
	for x in seq.deref() {
		if callable.call(vm, vec![x.clone()])?.get_bool()? {
			filtered.push(x.clone());
		}
	}
	
	if args[0].is::<Tuple>() {
		Ok(vm.gc.add_imm(Tuple(filtered)))
	} else if args[0].is::<List>() {
		Ok(vm.gc.add_mut(List(filtered)))
	} else {
		unimplemented!()
	}
});

native_func!(tuple_to_list, vm, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let tuple = args[0].get_imm::<Tuple>()?;
	Ok(vm.gc.add_mut(List(tuple.0.clone())))
});

native_func!(list_push, args, {
	check_arg_cnt(1, args.len() - 1)?;
	let list = args[0].get_mut::<List>()?;
	list.borrow_mut().0.push(args[1].clone());
	Ok(Value::Nil)
});

native_func!(list_pop, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let list = args[0].get_mut::<List>()?;
	let mut list = list.borrow_mut();
	if let Some(val) = list.0.pop() {
		Ok(val)
	} else {
		Err(String::from("Cannot pop empty list"))
	}
});

native_func!(list_insert, args, {
	check_arg_cnt(2, args.len() - 1)?;
	let list = args[0].get_mut::<List>()?;
	let len = list.borrow().0.len();
	let idx = args[2].get_int()?;
	if let Some(idx) = usize::try_from(idx).ok()
			.filter(|idx| *idx <= len) {
		list.borrow_mut().0.insert(idx, args[1].clone());
		Ok(Value::Nil)
	} else {
		Err(format!("Cannot insert at position {} in list of length {}", idx, len))
	}
});

native_func!(list_remove, args, {
	check_arg_cnt(1, args.len() - 1)?;
	let list = args[0].get_mut::<List>()?;
	let len = list.borrow().0.len();
	let idx = args[1].get_int()?;
	if let Some(idx) = usize::try_from(idx).ok()
			.filter(|idx| *idx < len) {
		list.borrow_mut().0.remove(idx);
		Ok(Value::Nil)
	} else {
		Err(format!("Cannot remove position {} in list of length {}", idx, len))
	}
});

native_func!(list_to_tuple, vm, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let list = args[0].get_mut::<List>()?;
	let list = list.borrow();
	Ok(vm.gc.add_imm(Tuple(list.0.clone())))
});

native_func!(map_contains, args, {
	check_arg_cnt(1, args.len() - 1)?;
	let map = args[0].get_mut::<Map>()?;
	let map = map.borrow();
	Ok(Value::Bool(map.contains_key(&args[1])))
});

native_func!(map_remove, args, {
	check_arg_cnt(1, args.len() - 1)?;
	let map = args[0].get_mut::<Map>()?;
	let mut map = map.borrow_mut();
	match map.remove(&args[1]) {
		Some(val) => Ok(val),
		None => Ok(Value::Nil),
	}
});

native_func!(map_size, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let map = args[0].get_mut::<Map>()?;
	let map = map.borrow();
	Ok(Value::Int(map.len() as i32))
});

native_func!(map_pairs, vm, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let map = args[0].get_mut::<Map>()?;
	let map = map.borrow();
	let list: Vec<Value> = map
		.iter()
		.map(|(k,v)| vm.gc.add_imm(Tuple(vec![k.clone(), v.clone()])))
		.collect();
	Ok(vm.gc.add_mut(List(list)))
});

struct CharIterator {
	string: String,
	offset: usize,
	cursor: GraphemeCursor,
}
unsafe impl Primitive for CharIterator {}

impl CharIterator {
	fn new(string: String) -> Self {
		let len = string.len();
		CharIterator {
			string,
			offset: 0,
			cursor: GraphemeCursor::new(0, len, true)
		}
	}
}

impl NativeIterator for CharIterator {
	fn next(&mut self, _vm: &mut VmArena) -> Result<Option<Value>, String> {
		let next = self.cursor.next_boundary(&self.string, 0)
			.map_err(|_| String::from("Error during iteration over string"))?;
		match next {
			Some(next) => {
				let val = Value::from(self.string[self.offset .. next].to_string());
				self.offset = next;
				Ok(Some(val))
			},
			None => Ok(None),
		}
	}
}

native_func!(str_size, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let s = args[0].get_string()?;
	Ok(Value::Int(s.graphemes(true).count() as i32))
});

native_func!(str_chars, vm, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let s = args[0].get_string()?;
	Ok(Value::from_native_iter(&mut vm.gc, CharIterator::new(s)))
});

native_func!(str_split, vm, args, {
	check_arg_cnt(1, args.len() - 1)?;
	let s = args[0].get_string()?;
	let pat = args[1].get_string()?;
	let parts: Vec<Value> = s.split(&pat).map(|s| Value::String(String::from(s).into_boxed_str())).collect();
	Ok(vm.gc.add_mut(List(parts)))
});

native_func!(str_contains, args, {
	check_arg_cnt(1, args.len() - 1)?;
	let s = args[0].get_string()?;
	let pat = args[1].get_string()?;
	Ok(Value::Bool(s.contains(&pat)))
});

native_func!(str_parse_int, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let s = args[0].get_string()?;
	match s.parse() {
		Ok(i) => Ok(Value::Int(i)),
		Err(_) => Ok(Value::Nil),
	}
});

native_func!(iter_join, vm, args, {
	if args.len() > 2 {
		return Err(expected("0/1 arguments", &(args.len() - 1).to_string()));
	}
	let iter = args[0].get_iter()?;
	let mut iter = iter.borrow_mut();
	let sep = if args.len() == 2 {
		Some(args[1].get_string()?)
	} else {
		None
	};
	
	let mut buf = String::new();
	let mut first = true;
	while let Some(val) = iter.next(vm)? {
		if first {
			first = false;
		} else if let Some(ref sep) = sep {
			buf += &sep;
		}
		buf += &val.get_string()?;
	}
	
	Ok(Value::from(buf))
});

native_func!(iter_to_list, vm, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let iter = args[0].get_iter()?;
	let mut iter = iter.borrow_mut();
	
	let mut values = vec![];
	while let Some(val) = iter.next(vm)? {
		values.push(val);
	}
	
	Ok(vm.gc.add_mut(List(values)))
});
native_func!(iter_to_tuple, vm, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let iter = args[0].get_iter()?;
	let mut iter = iter.borrow_mut();
	
	let mut values = vec![];
	while let Some(val) = iter.next(vm)? {
		values.push(val);
	}
	
	Ok(vm.gc.add_imm(Tuple(values)))
});

native_func!(iter_next, vm, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let iter = args[0].get_iter()?;
	let mut iter = iter.borrow_mut();
	
	let res = match iter.next(vm)? {
		Some(val) => vec![Value::Bool(true), val],
		None => vec![Value::Bool(false)],
	};
	Ok(vm.gc.add_imm(Tuple(res)))
});

#[derive(Trace)]
struct MapIterator {
	before: GcCell<dyn NativeIterator>,
	func: Callable,
}

impl NativeIterator for MapIterator {
	fn next(&mut self, vm: &mut VmArena) -> Result<Option<Value>, String> {
		match self.before.borrow_mut().next(vm)? {
			Some(x) => Ok(Some(self.func.call(vm, vec![x])?)),
			None => Ok(None),
		}
	}
}

native_func!(iter_map, vm, args, {
	check_arg_cnt(1, args.len() - 1)?;
	let before = args[0].get_iter()?;
	let func = args[1].get_callable()?;
	
	Ok(Value::Iterator(vm.gc.add_cell(MapIterator {
		before, func,
	})))
});

#[derive(Trace)]
struct FilterIterator {
	before: GcCell<dyn NativeIterator>,
	func: Callable,
}

impl NativeIterator for FilterIterator {
	fn next(&mut self, vm: &mut VmArena) -> Result<Option<Value>, String> {
		loop {
			match self.before.borrow_mut().next(vm)? {
				Some(x) => {
					if self.func.call(vm, vec![x.clone()])?.get_bool()? {
						return Ok(Some(x));
					}
				},
				None => return Ok(None),
			}
		}
	}
}

native_func!(iter_filter, vm, args, {
	check_arg_cnt(1, args.len() - 1)?;
	let before = args[0].get_iter()?;
	let func = args[1].get_callable()?;
	
	Ok(Value::Iterator(vm.gc.add_cell(FilterIterator {
		before, func,
	})))
});


type NativeFn = fn(&mut VmArena, Vec<Value>) -> Result<Value, String>;

pub static FUNCTIONS: Lazy<HashMap<String, NativeFn>> = Lazy::new(|| [
	("repr", repr as NativeFn),
	("xrange", xrange),
	("range", range),
	("type", type_),
	("read_file", read_file),
	("error", error),
].iter().map(|(s,f)| (s.to_string(), *f)).collect());

pub static MODULES: Lazy<HashMap<String, HashMap<String, NativeFn>>> = Lazy::new(|| [
	("term", vec![
		("log", log as NativeFn),
		("writeln", writeln),
	]),
	("file", vec![
		("read", read_file as NativeFn),
	]),
	("rand", vec![
		("range", rand_range as NativeFn),
	]),
].iter().map(|(m,p)| (m.to_string(), p.iter().map(|(s,f)| (s.to_string(), *f)).collect())).collect());

pub static GLOBAL_NAMES: Lazy<HashSet<String>> = Lazy::new(||
	FUNCTIONS.keys().cloned().chain(MODULES.keys().cloned()).collect());

pub static METHODS: Lazy<HashMap<Type, HashMap<String, NativeFn>>> = Lazy::new(|| [
	(Type::Object(Tuple::DYN_TYPE), vec![
		("size", seq_size as NativeFn),
		("sub", seq_sub),
		("to_iter", seq_to_iter),
		("map", seq_map),
		("filter", seq_filter),
		("to_list", tuple_to_list),
	]),
	(Type::Object(List::DYN_TYPE), vec![
		("push", list_push as NativeFn),
		("pop", list_pop),
		("insert", list_insert),
		("remove", list_remove),
		("size", seq_size),
		("sub", seq_sub),
		("to_iter", seq_to_iter),
		("map", seq_map),
		("filter", seq_filter),
		("to_tuple", list_to_tuple),
	]),
	(Type::Map, vec![
		("contains", map_contains as NativeFn),
		("remove", map_remove),
		("size", map_size),
		("pairs", map_pairs),
	]),
	(Type::String, vec![
		("size", str_size as NativeFn),
		("chars", str_chars),
		("split", str_split),
		("contains", str_contains),
		("parse_int", str_parse_int),
	]),
	(Type::Iterator, vec![
		("join", iter_join as NativeFn),
		("to_list", iter_to_list),
		("to_tuple", iter_to_tuple),
		("next", iter_next),
		("map", iter_map),
		("filter", iter_filter),
	])
].iter().map(|(t,p)| (*t, p.iter().map(|(s,f)| (s.to_string(), *f)).collect())).collect());

pub fn create(globals: &mut HashMap<String, Value>, gc: &mut GcHeap) {
	for (name, func) in FUNCTIONS.deref() {
		globals.insert(name.clone(), Value::from_native_func(gc, func, None));
	}
	for (name, methods) in MODULES.deref() {
		let mut map = HashMap::new();
		for (name, func) in methods {
			map.insert(name.clone(), Value::from_native_func(gc, func, None));
		}
		globals.insert(name.clone(), Value::Struct(gc.add_cell(map)));
	}
}
