
use std::io::{self, Write};
use std::rc::Rc;

use rustyline::{Editor, error::ReadlineError};

use crate::ast::{Statement, PositionedStatement};
use crate::colors::*;
use crate::compiler::InteractiveContext;
use crate::grammar::parse_partial_stat;
use crate::print_err;
use crate::vm::InteractiveVm;

pub struct Repl {
	inter_ctx: InteractiveContext,
	vm: InteractiveVm,
	show_bytecode: bool,
	rl: rustyline::Editor<()>,
}

impl Repl {
	pub fn new(show_bytecode: bool) -> Self {
		Repl {
			inter_ctx: InteractiveContext::new(),
			vm: InteractiveVm::new(),
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
	
	fn run_stat(&mut self, stat: Statement) -> Result<(), String> {
		
		let mut func_ctx = self.inter_ctx.make_func_ctx();
		
		if let Statement::ExprStat(expr) = stat {
			func_ctx.compile_expression_log(expr)?;
		} else {
			func_ctx.compile_statement(PositionedStatement(1, stat))?;
		}
		
		if self.show_bytecode {
			println!(": {:?}", &func_ctx.func.code);
		}
		
		self.vm.execute_function(Rc::new(func_ctx.func.clone()))?;
		
		// If compilation and execution were successful,
		// store global context changes for next instruction
		self.inter_ctx.apply_func_ctx(func_ctx);
		
		Ok(())
	}

	pub fn start(&mut self) {
		println!("{}Cantan REPL{}", BRIGHT_WHITE, RESET);
		if self.show_bytecode {
			println!("(Showing bytecode)");
		}
		println!();
		
		while let Some(stat) = self.read_statement() {
			if let Err(err) = self.run_stat(stat) {
				print_err(&err.trim_end());
			}
			println!();
		}
		
		println!("Bye!");
	}
}
