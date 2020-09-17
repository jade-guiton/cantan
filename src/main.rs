#[macro_use]
extern crate gc_arena;

mod ast;
mod colors;
mod compiler;
mod chunk;
mod cli;
mod grammar;
mod repl;
mod util;
mod value;
mod vm;


use std::env;
use std::rc::Rc;

use cli::{CommandSpec, parse_args};
use colors::*;


const USAGE: &str = "
Usage:
  cantan parse <src>
  cantan compile <src>
  cantan run <src>
  cantan repl [-d]
  cantan --help | -h

Arguments:
  <src>        Path to a Cantan source file (usually .cn)

Options:
  --help, -h   Print this help message
  -d           Show run bytecode
";

static COMMANDS: &[CommandSpec] = &[
	CommandSpec::new("parse", true, &[]),
	CommandSpec::new("compile", true, &[]),
	CommandSpec::new("run", true, &[]),
	CommandSpec::new("repl", false, &[("-d", false)]),
	CommandSpec::new("--help", false, &[]),
	CommandSpec::new("-h", false, &[]),
];

fn print_err(err: &str) {
	println!("{}{}{}", BRIGHT_RED, err, RESET);
}

fn parse(path: &str) -> Option<ast::Block> {
	match grammar::parse(path) {
		Ok(res) => Some(res),
		Err(err) => {
			print_err(&err);
			None
		}
	}
}

fn compile(path: &str) -> Option<chunk::CompiledFunction> {
	let prog = parse(path)?;
	match compiler::compile_program(prog) {
		Ok(res) => Some(res),
		Err(err) => {
			print_err(&err);
			None
		}
	}
}


fn main() {
	let args = env::args();
	match parse_args(COMMANDS, args) {
		Ok(cmd) => {
			match cmd.name {
				"parse" => {
					if let Some(prog) = parse(&cmd.file.unwrap()) {
						println!("{:?}", prog);
					}
				},
				"compile" => {
					if let Some(prog) = compile(&cmd.file.unwrap()) {
						prog.list();
					}
				},
				"repl" => {
					let mut repl = repl::Repl::new(cmd.options.contains("-d"));
					repl.start();
				},
				"run" => {
					if let Some(prog) = compile(&cmd.file.unwrap()) {
						if let Err(err) = vm::execute_function(Rc::new(prog)) {
							print_err(&err);
						}
					}
				},
				"--help" | "-h" => println!("{}", USAGE),
				_ => panic!("Unimplemented command"),
			}
		},
		Err(err) => {
			eprintln!("{}{}{}\n{}", BRIGHT_RED, err, RESET, USAGE);
		}
	}
}
