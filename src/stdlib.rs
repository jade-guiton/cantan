use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use std::ops::Deref;

use gc_arena::{GcCell, MutationContext};
use once_cell::sync::Lazy;
use unicode_segmentation::UnicodeSegmentation;

use crate::value::{expected, NativeIterator, Type, Value};

// To avoid writing the same type signature every time
macro_rules! native_func {
	($id:ident, $mc:ident, $args:ident, $block:block) => {
		fn $id<'gc>($mc: MutationContext<'gc, '_>, $args: Vec<Value<'gc>>) -> Result<Value<'gc>, String> {
			$block
		}
	};
	($id:ident, $args:ident, $block:block) => { native_func!($id, _mc, $args, $block); };
	($id:ident, $block:block) => { native_func!($id, _mc, _args, $block); };
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

native_func!(list_push, mc, args, {
	check_arg_cnt(1, args.len() - 1)?;
	let list = args[0].get_list()?;
	list.write(mc).push(args[1].clone());
	Ok(Value::Nil)
});

native_func!(list_insert, mc, args, {
	check_arg_cnt(2, args.len() - 1)?;
	let list = args[0].get_list()?;
	let len = list.read().len();
	let idx = args[2].get_int()?;
	if let Some(idx) = usize::try_from(idx).ok()
			.filter(|idx| *idx <= len) {
		list.write(mc).insert(idx, args[1].clone());
		Ok(Value::Nil)
	} else {
		Err(format!("Cannot insert at position {} in list of length {}", idx, len))
	}
});

native_func!(list_pop, mc, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let list = args[0].get_list()?;
	let mut list = list.write(mc);
	if let Some(val) = list.pop() {
		Ok(val)
	} else {
		Err(String::from("Cannot pop empty list"))
	}
});

native_func!(list_len, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let list = args[0].get_list()?;
	let len = i32::try_from(list.read().len())
		.map_err(|_| String::from("List length does not fit in integer"))?;
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

native_func!(list_sub, mc, args, {
	if args.len() < 2 || args.len() > 3 {
		return expected("1/2 arguments", &(args.len() - 1).to_string());
	}
	let list = args[0].get_list()?;
	let list = list.read();
	let start = args[1].get_int()?;
	let start2 = wrap_index(start, list.len());
	let end = args.get(2).map(|v| v.get_int()).transpose()?;
	let end2 = end.map(|idx| wrap_index(idx, list.len()))
		.unwrap_or_else(|| Some(list.len()));
	let sub = start2.and_then(|start| end2.and_then(|end| list.get(start..end)))
		.ok_or_else(||
			format!("Cannot take sublist {}..{} of list of length {}",
				start, end.map(|i| i.to_string()).unwrap_or(String::new()), list.len())
		)?.to_vec();
	Ok(Value::List(GcCell::allocate(mc, sub)))
});

#[derive(Collect)]
#[collect(no_drop)]
struct IntIterator {
	next: Cell<i32>,
	until: i32,
	step: i32,
}

impl<'gc> NativeIterator<'gc> for IntIterator {
	fn next(&mut self, _mc: MutationContext<'gc, '_>) -> Result<Option<Value<'gc>>, String> {
		let next = self.next.get();
		if next < self.until {
			self.next.set(next + self.step);
			Ok(Some(Value::Int(next)))
		} else {
			Ok(None)
		}
	}
}

fn check_range_args(args: &Vec<Value>) -> Result<(Option<i32>, i32, i32), String> {
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

native_func!(xrange, mc, args, {
	let (start, until, step) = check_range_args(&args)?;
	Ok(Value::from_native_iter(mc, IntIterator {
		next: Cell::new(start.unwrap_or(0)),
		until,
		step
	}))
});

native_func!(range, mc, args, {
	let (start, until, step) = check_range_args(&args)?;
	Ok(Value::from_native_iter(mc, IntIterator {
		next: Cell::new(start.unwrap_or(1)),
		until: until + 1,
		step
	}))
});

native_func!(str_chars, mc, args, {
	check_arg_cnt(0, args.len() - 1)?;
	let s = args[0].get_string()?;
	let chars: Vec<Value<'gc>> = s.graphemes(true)
		.map(|s| Value::from(s.to_string())).collect();
	Ok(Value::List(GcCell::allocate(mc, chars)))
});


type NativeFn = for<'gc> fn(MutationContext<'gc, '_>, Vec<Value<'gc>>) -> Result<Value<'gc>, String>;

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
	(Type::List, vec![
		("push", list_push as NativeFn),
		("insert", list_insert),
		("pop", list_pop),
		("len", list_len),
		("sub", list_sub),
	]),
	(Type::String, vec![
		("chars", str_chars as NativeFn),
	]),
].iter().map(|(t,p)| (*t, p.iter().map(|(s,f)| (s.to_string(), *f)).collect())).collect());

pub fn create<'gc>(globals: &mut HashMap<String, Value<'gc>>, mc: MutationContext<'gc, '_>) {
	for (name, func) in FUNCTIONS.deref() {
		globals.insert(name.clone(), Value::from_native_func(mc, func, None));
	}
}
