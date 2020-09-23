
use std::collections::{HashMap, HashSet};
use std::ops::Deref;

use gc_arena::MutationContext;
use once_cell::sync::Lazy;

use crate::value::Value;

// To avoid writing the same type signature every time
macro_rules! native_func {
	($id:ident, $mc:ident, $args:ident, $block:block) => {
		fn $id<'gc>($mc: MutationContext<'gc, '_>, $args: Vec<Value<'gc>>) -> Result<Value<'gc>, String> {
			$block
		}
	};
	($id:ident, $args:ident, $block:block) => {
		fn $id<'gc>(_: MutationContext<'gc, '_>, $args: Vec<Value<'gc>>) -> Result<Value<'gc>, String> {
			$block
		}
	};
	($id:ident, $block:block) => {
		fn $id<'gc>(_: MutationContext<'gc, '_>, _: Vec<Value<'gc>>) -> Result<Value<'gc>, String> {
			$block
		}
	};
}

fn check_arg_cnt(exp: usize, args: &Vec<Value<'_>>) -> Result<(), String> {
	if args.len() != exp {
		crate::value::expected(&format!("{} arguments", exp), &format!("{}", args.len()))
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
	check_arg_cnt(1, &args)?;
	let val = args[0].get_string()?;
	println!("{}", val);
	Ok(Value::Nil)
});

native_func!(repr, args, {
	check_arg_cnt(1, &args)?;
	Ok(Value::from(args[0].repr()))
});

type NativeFn = for<'gc> fn(MutationContext<'gc, '_>, Vec<Value<'gc>>) -> Result<Value<'gc>, String>;

pub static FUNCTIONS: Lazy<HashMap<String, NativeFn>> = Lazy::new(|| [
	("log", log as NativeFn),
	("writeln", writeln),
	("repr", repr),
].iter().map(|(s,f)| (s.to_string(), *f)).collect());

pub static GLOBAL_NAMES: Lazy<HashSet<String>> = Lazy::new(|| FUNCTIONS.keys().cloned().collect());

pub fn create<'gc>(globals: &mut HashMap<String, Value<'gc>>, mc: MutationContext<'gc, '_>) {
	for (name, func) in FUNCTIONS.deref() {
		globals.insert(name.clone(), Value::from_native_func(mc, func));
	}
}
