
use std::collections::{HashMap, HashSet};

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

pub static GLOBALS: Lazy<HashSet<String>> = Lazy::new(|| [
	"log", "writeln", "repr",
].iter().map(|s| s.to_string()).collect());

pub fn create<'gc>(globals: &mut HashMap<String, Value<'gc>>, mc: MutationContext<'gc, '_>) {
	globals.insert("log".to_string(), Value::from_native_func(mc, log));
	globals.insert("writeln".to_string(), Value::from_native_func(mc, writeln));
	globals.insert("repr".to_string(), Value::from_native_func(mc, repr));
}
