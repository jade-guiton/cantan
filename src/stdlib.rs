use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::iter::Iterator;
use std::ops::Deref;

use once_cell::sync::Lazy;
use unicode_segmentation::GraphemeCursor;

use crate::value::{expected, NativeIterator, Type, Value};
use crate::gc::{Trace, Primitive, GCHeap};

// To avoid writing the same type signature every time
macro_rules! native_func {
	($id:ident, $gc:ident, $args:ident, $block:block) => {
		fn $id($gc: &mut GCHeap, $args: Vec<Value>) -> Result<Value, String> {
			$block
		}
	};
	($id:ident, $args:ident, $block:block) => { native_func!($id, _gc, $args, $block); };
	($id:ident, $block:block) => { native_func!($id, _gc, _args, $block); };
}

fn check_arg_cnt(exp: usize, cnt: usize) -> Result<(), String> {
	if cnt != exp {
		expected(&format!("{} arguments", exp), &cnt.to_string())
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

native_func!(seq_size, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let seq = args[0].get_sequence()?;
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

native_func!(seq_sub, gc, args, {
	if args.len() < 2 || args.len() > 3 {
		return expected("1/2 arguments", &(args.len() - 1).to_string());
	}
	let seq = args[0].get_sequence()?;
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
	match &args[0] {
		Value::Tuple(_) => Ok(Value::Tuple(gc.add(sub))),
		Value::List(_) => Ok(Value::List(gc.add_cell(sub))),
		_ => unimplemented!(),
	}
});

native_func!(seq_to_iter, gc, args, {
	check_arg_cnt(0, args.len() - 1)?;
	args[0].make_iter(gc).transpose().unwrap()
});

native_func!(tuple_to_list, gc, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let tuple = args[0].get_tuple()?;
	Ok(Value::List(gc.add_cell(tuple.deref().clone())))
});

native_func!(list_push, args, {
	check_arg_cnt(1, args.len() - 1)?;
	let list = args[0].get_list()?;
	list.borrow_mut().push(args[1].clone());
	Ok(Value::Nil)
});

native_func!(list_insert, args, {
	check_arg_cnt(2, args.len() - 1)?;
	let list = args[0].get_list()?;
	let len = list.borrow().len();
	let idx = args[2].get_int()?;
	if let Some(idx) = usize::try_from(idx).ok()
			.filter(|idx| *idx <= len) {
		list.borrow_mut().insert(idx, args[1].clone());
		Ok(Value::Nil)
	} else {
		Err(format!("Cannot insert at position {} in list of length {}", idx, len))
	}
});

native_func!(list_pop, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let list = args[0].get_list()?;
	let mut list = list.borrow_mut();
	if let Some(val) = list.pop() {
		Ok(val)
	} else {
		Err(String::from("Cannot pop empty list"))
	}
});

native_func!(list_to_tuple, gc, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let list = args[0].get_list()?;
	let list = list.borrow();
	Ok(Value::Tuple(gc.add(list.deref().clone())))
});

#[derive(Trace)]
struct IntIterator {
	next: Cell<i32>,
	until: i32,
	step: i32,
}

impl NativeIterator for IntIterator {
	fn next(&mut self, _gc: &mut GCHeap) -> Result<Option<Value>, String> {
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
		return expected("1-3 arguments", &args.len().to_string());
	}
	let (start, until) = if args.len() == 1 {
		(None, args[0].get_int()?)
	} else {
		(Some(args[0].get_int()?), args[1].get_int()?)
	};
	let step = if args.len() == 3 { args[2].get_int()? } else { 1 };
	Ok((start, until, step))
}

native_func!(xrange, gc, args, {
	let (start, until, step) = check_range_args(&args)?;
	Ok(Value::from_native_iter(gc, IntIterator {
		next: Cell::new(start.unwrap_or(0)),
		until,
		step
	}))
});

native_func!(range, gc, args, {
	let (start, until, step) = check_range_args(&args)?;
	Ok(Value::from_native_iter(gc, IntIterator {
		next: Cell::new(start.unwrap_or(1)),
		until: until + 1,
		step
	}))
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
	fn next(&mut self, _gc: &mut GCHeap) -> Result<Option<Value>, String> {
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

native_func!(str_chars, gc, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let s = args[0].get_string()?;
	Ok(Value::from_native_iter(gc, CharIterator::new(s)))
});

native_func!(iter_join, gc, args, {
	if args.len() > 2 {
		return expected("0/1 arguments", &(args.len() - 1).to_string());
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
	while let Some(val) = iter.iter.next(gc)? {
		if first {
			first = false;
		} else if let Some(ref sep) = sep {
			buf += &sep;
		}
		buf += &val.get_string()?;
	}
	
	Ok(Value::from(buf))
});

native_func!(iter_to_list, gc, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let iter = args[0].get_iter()?;
	let mut iter = iter.borrow_mut();
	
	let mut values = vec![];
	while let Some(val) = iter.iter.next(gc)? {
		values.push(val);
	}
	
	Ok(Value::List(gc.add_cell(values)))
});
native_func!(iter_to_tuple, gc, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let iter = args[0].get_iter()?;
	let mut iter = iter.borrow_mut();
	
	let mut values = vec![];
	while let Some(val) = iter.iter.next(gc)? {
		values.push(val);
	}
	
	Ok(Value::Tuple(gc.add(values)))
});

native_func!(iter_next, gc, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let iter = args[0].get_iter()?;
	let mut iter = iter.borrow_mut();
	
	let res = match iter.iter.next(gc)? {
		Some(val) => vec![Value::Bool(true), val],
		None => vec![Value::Bool(false)],
	};
	Ok(Value::Tuple(gc.add(res)))
});


type NativeFn = fn(&mut GCHeap, Vec<Value>) -> Result<Value, String>;

pub static FUNCTIONS: Lazy<HashMap<String, NativeFn>> = Lazy::new(|| [
	("log", log as NativeFn),
	("writeln", writeln),
	("repr", repr),
	("xrange", xrange),
	("range", range),
].iter().map(|(s,f)| (s.to_string(), *f)).collect());

pub static GLOBAL_NAMES: Lazy<HashSet<String>> = Lazy::new(||
	FUNCTIONS.keys().cloned().collect());

pub static METHODS: Lazy<HashMap<Type, HashMap<String, NativeFn>>> = Lazy::new(|| [
	(Type::Tuple, vec![
		("size", seq_size as NativeFn),
		("sub", seq_sub),
		("to_iter", seq_to_iter),
		("to_list", tuple_to_list),
	]),
	(Type::List, vec![
		("push", list_push as NativeFn),
		("insert", list_insert),
		("pop", list_pop),
		("size", seq_size),
		("sub", seq_sub),
		("to_iter", seq_to_iter),
		("to_tuple", list_to_tuple),
	]),
	(Type::String, vec![
		("chars", str_chars as NativeFn),
	]),
	(Type::Iterator, vec![
		("join", iter_join as NativeFn),
		("to_list", iter_to_list),
		("to_tuple", iter_to_tuple),
		("next", iter_next),
	])
].iter().map(|(t,p)| (*t, p.iter().map(|(s,f)| (s.to_string(), *f)).collect())).collect());

pub fn create(globals: &mut HashMap<String, Value>, gc: &mut GCHeap) {
	for (name, func) in FUNCTIONS.deref() {
		globals.insert(name.clone(), Value::from_native_func(gc, func, None));
	}
}
