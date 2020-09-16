use std::io::{self, Write};

use rustyline::{Editor, error::ReadlineError};

use crate::ast::Statement;
use crate::compiler::{BlockType, FunctionContext};
use crate::grammar::parse_partial_stat;
use crate::vm::Vm;
use crate::print_err;

const BRIGHT_WHITE: &str = "\u{001b}[37;1m";
const RESET: &str = "\u{001b}[0m";

pub struct Repl {
	func_ctx: FunctionContext,
	code_idx: usize,
	vm: Vm,
	show_bytecode: bool,
	rl: rustyline::Editor<()>,
}

impl Repl {
	pub fn new(show_bytecode: bool) -> Self {
		let mut func_ctx = FunctionContext::new(vec![]).unwrap();
		func_ctx.start_block(BlockType::FunctionMain);
		Repl {
			func_ctx,
			code_idx: 0,
			vm: Vm::new(),
			show_bytecode,
			rl: Editor::<()>::with_config(rustyline::Config::builder()
				.auto_add_history(true).build()),
		}
	}
	
	fn read_statement(&mut self) -> Option<Statement> {
		let mut buffer = String::new();
		let mut new_stat = true;
		loop {
			let res = if new_stat {
				new_stat = false;
				self.rl.readline(&format!("{}> ", BRIGHT_WHITE))
			} else {
				self.rl.readline(&format!("{}| ", BRIGHT_WHITE))
			};
			print!("{}", RESET);
			io::stdout().flush().unwrap();
			
			match res {
				Ok(line) => {
					buffer += &(line + "\n");
					if buffer.trim() == "" {
						buffer.clear();
						new_stat = true;
					} else if buffer.trim() == "exit" {
						return None
					} else if let Some(res) = parse_partial_stat(&buffer) {
						match res {
							Ok(stat) => return Some(stat),
							Err(err) => {
								print_err(&err);
								buffer.clear();
								new_stat = true;
							}
						}
					}
				},
				Err(err) => {
					match err {
						ReadlineError::Interrupted => println!("{}^C{}", BRIGHT_WHITE, RESET),
						ReadlineError::Eof => println!("{}^D{}", BRIGHT_WHITE, RESET),
						_ => print_err(&format!("{}", err)),
					};
					return None;
				},
			}
		}
	}
	
	fn compile_stat(&mut self, stat: Statement) -> Result<(), String> {
		if let Statement::ExprStat(expr) = stat {
			self.func_ctx.compile_expression(expr)
				.map(|()| self.func_ctx.add_log())
		} else {
			self.func_ctx.compile_statement(stat)
		}
	}
	
	fn run_stat(&mut self, stat: Statement) {
		let old_ctx = self.func_ctx.clone();
		if let Err(err) = self.compile_stat(stat) {
			print_err(&err);
			self.func_ctx = old_ctx; // Roll back compiler state
			return;
		}
		
		let code_end = self.func_ctx.func.code.len();
		
		if self.show_bytecode {
			println!(": {:?}", &self.func_ctx.func.code[self.code_idx..code_end]);
		}
		
		if let Err(err) = self.vm.execute_code(&self.func_ctx.func, self.code_idx..code_end) {
			print_err(&err);
		}
		
		self.code_idx = code_end;
	}

	pub fn start(&mut self) {
		println!("{}Cantan REPL{}", BRIGHT_WHITE, RESET);
		if self.show_bytecode {
			println!("(Showing bytecode)");
		}
		println!();
		
		while let Some(stat) = self.read_statement() {
			self.run_stat(stat);
			println!();
		}
		
		println!("Bye!");
	}
}
