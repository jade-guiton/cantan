use std::collections::HashMap;
use std::convert::TryFrom;

use crate::ast::*;
use crate::chunk::*;
use crate::util::IntSet;

pub struct StatementContext<'a> {
	func_ctx: &'a FunctionContext,
	code: Vec<Instr>,
	csts: Vec<Primitive>,
	classes: Vec<Vec<String>>,
	used_regs: Vec<u16>,
	locals: Vec<(String, u16)>,
	stack_size: usize,
}

impl<'a> StatementContext<'a> {
	pub fn new(func_ctx: &'a FunctionContext) -> Self {
		StatementContext {
			func_ctx,
			code: vec![],
			csts: vec![],
			classes: vec![],
			used_regs: vec![],
			locals: vec![],
			stack_size: 0
		}
	}
	
	fn compile_sequence(&mut self, values: Vec<Expr>, mutable: bool) -> Result<(), String> {
		let len = u16::try_from(values.len())
			.map_err(|_| format!("{} is too long", if mutable { "List" } else { "Tuple"} ))?;
		for expr in values {
			self.compile_expression(expr)?;
		}
		if mutable {
			self.code.push(Instr::NewList(len));
		} else {
			self.code.push(Instr::NewTuple(len));
		}
		self.stack_size -= len as usize;
		self.stack_size += 1;
		Ok(())
	}
	
	fn add_prim_cst(&mut self, cst: Primitive) -> Result<u16, String> {
		let mut existing_csts = self.func_ctx.func.csts.iter().chain(self.csts.iter());
		if let Some(idx) = existing_csts.position(|cst2| &cst == cst2) {
			Ok(idx as u16)
		} else {
			let idx = u16::try_from(self.func_ctx.func.csts.len() + self.csts.len())
				.map_err(|_| String::from("Too many constants"))?;
			self.csts.push(cst);
			Ok(idx)
		}
	}

	fn compile_expression(&mut self, expr: Expr) -> Result<(), String> {
		match expr {
			Expr::Primitive(cst) => {
				let idx = self.add_prim_cst(cst)?;
				self.code.push(Instr::Constant(idx));
				self.stack_size += 1;
			},
			Expr::Tuple(values) => self.compile_sequence(values, false)?,
			Expr::List(values) => self.compile_sequence(values, true)?,
			Expr::Map(pairs) => {
				let len = u16::try_from(pairs.len())
					.map_err(|_| String::from("Map is too large"))?;
				for (key, value) in pairs {
					self.compile_expression(key)?;
					self.compile_expression(value)?;
				}
				self.code.push(Instr::NewMap(len));
				self.stack_size -= 2*len as usize;
				self.stack_size += 1;
			},
			Expr::Object(pairs) => {
				let len = u16::try_from(pairs.len())
					.map_err(|_| String::from("Object is too large"))?;
				let mut class = vec![];
				for (id, value) in pairs {
					class.push(id);
					self.compile_expression(value)?;
				}
				let idx = self.func_ctx.func.classes.iter().position(|c| c == &class)
					.unwrap_or_else(|| {
						let idx = self.func_ctx.func.classes.len() + self.classes.len();
						self.classes.push(class);
						idx
					});
				let idx = u16::try_from(idx).map_err(|_| String::from("Too many object classes"))?;
				self.code.push(Instr::NewObject(idx));
				self.stack_size -= len as usize;
				self.stack_size += 1;
			},
			Expr::LExpr(lexpr) => {
				match lexpr {
					LExpr::Id(id) => {
						match self.func_ctx.locals.get(&id).copied() {
							Some(reg) => {
								self.code.push(Instr::Load(reg));
								self.stack_size += 1;
							},
							None => return Err(format!("Referencing undefined local '{}'", id)),
						}
					},
					LExpr::Index(seq, idx) => {
						self.compile_expression(*seq)?;
						self.compile_expression(*idx)?;
						self.code.push(Instr::Index);
						self.stack_size -= 1;
					},
					LExpr::Prop(obj, prop) => {
						self.compile_expression(*obj)?;
						let cst_idx = self.add_prim_cst(Primitive::String(prop))?;
						self.code.push(Instr::Prop(cst_idx));
					},
				}
			},
			Expr::Binary(op, expr1, expr2) => {
				self.compile_expression(*expr1)?;
				self.compile_expression(*expr2)?;
				self.code.push(Instr::Binary(op));
				self.stack_size -= 1;
			},
			_ => { todo!() },
		}
		Ok(())
	}

	pub fn compile_statement(&mut self, stat: Statement) -> Result<(), String> {
		match stat {
			Statement::Let(lexpr, expr) => {
				match lexpr {
					LExpr::Id(id) => {
						let reg = {
							if let Some(reg) = self.func_ctx.locals.get(&id).copied() { // Shadowing previous binding
								self.code.push(Instr::Drop(reg));
								reg
							} else {
								let reg = self.func_ctx.used_regs.find_hole();
								self.used_regs.push(reg);
								self.locals.push((id, reg));
								reg
							}
						};
						
						self.compile_expression(expr)?;
						
						self.code.push(Instr::Store(reg));
						self.stack_size -= 1;
					},
					_ => { todo!() },
				}
			},
			Statement::Set(lexpr, expr) => {
				self.compile_expression(expr)?;
				match lexpr {
					LExpr::Id(id) => {
						if let Some(reg) = self.func_ctx.locals.get(&id).copied() {
							self.code.push(Instr::Store(reg));
							self.stack_size -= 1;
						} else {
							return Err(format!("Assigning to undefined local '{}'", id));
						}
					},
					_ => { todo!() },
				}
			},
			Statement::ExprStat(expr) => {
				self.compile_expression(expr)?;
				self.code.push(Instr::Discard);
				self.stack_size -= 1;
			},
			_ => { todo!() }
		}
		
		assert!(self.stack_size == 0, "Compiler logic error: {} values remained on stack after statement compilation", self.stack_size);
		
		Ok(())
	}
}

pub struct FunctionContext {
	pub func: CompiledFunction,
	used_regs: IntSet<u16>,
	locals: HashMap<String, u16>,
}

impl FunctionContext {
	pub fn new(mut arg_names: Vec<String>) -> Result<Self, String> {
		let arg_cnt = u16::try_from(arg_names.len())
			.map_err(|_| String::from("Too many arguments in function"))?;
		
		let mut func_ctx = FunctionContext {
			func: CompiledFunction::new(arg_cnt),
			used_regs: IntSet::new(),
			locals: HashMap::new(),
		};
		
		for (idx, arg_name) in arg_names.drain(..).enumerate() {
			let reg = idx as u16;
			func_ctx.used_regs.add(reg);
			func_ctx.locals.insert(arg_name, reg);
		}
		
		Ok(func_ctx)
	}
	
	pub fn compile_expression(&mut self, expr: Expr) -> Result<(), String> {
		let mut stat_ctx = StatementContext::new(&self);
		stat_ctx.compile_expression(expr)?;
		
		let StatementContext { used_regs, locals, mut code, mut csts, mut classes, .. } = stat_ctx;
		for reg in used_regs {
			self.used_regs.add(reg);
		}
		for (id, reg) in locals {
			self.locals.insert(id, reg);
		}
		self.func.code.append(&mut code);
		self.func.csts.append(&mut csts);
		self.func.classes.append(&mut classes);
		
		Ok(())
	}
	
	pub fn compile_statement(&mut self, stat: Statement) -> Result<(), String> {
		let mut stat_ctx = StatementContext::new(&self);
		stat_ctx.compile_statement(stat)?;
		
		let StatementContext { used_regs, locals, mut code, mut csts, mut classes, .. } = stat_ctx;
		for reg in used_regs {
			self.used_regs.add(reg);
		}
		for (id, reg) in locals {
			self.locals.insert(id, reg);
		}
		self.func.code.append(&mut code);
		self.func.csts.append(&mut csts);
		self.func.classes.append(&mut classes);
		
		Ok(())
	}
	
	pub fn compile_function(mut self, block: Block) -> Result<CompiledFunction, String> {
		for stat in block {
			self.compile_statement(stat)?;
		}
		
		for (_id, reg) in self.locals.drain() {
			self.func.code.push(Instr::Drop(reg));
		}
		
		Ok(self.func)
	}
}

pub fn compile_program(prog: Block) -> Result<CompiledFunction, String> {
	FunctionContext::new(vec![])?.compile_function(prog)
}
