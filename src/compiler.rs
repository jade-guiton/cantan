use std::collections::HashMap;
use std::convert::TryFrom;
use std::rc::Rc;

use crate::ast::*;
use crate::chunk::*;
use crate::util::IntSet;


#[derive(Clone)]
pub struct BlockContext {
	breakable: bool,
	breaks: Vec<usize>,
	locals: HashMap<String, u16>,
}

#[derive(PartialEq)]
pub enum BlockType {
	FunctionMain,
	Normal,
	Breakable,
}

#[derive(Clone)]
pub struct FunctionContext {
	pub func: CompiledFunction,
	arg_names: Vec<String>,
	used_regs: IntSet<u16>,
	blocks: Vec<BlockContext>,
	stack_size: usize,
}

impl FunctionContext {
	pub fn new(arg_names: Vec<String>) -> Result<Self, String> {
		let arg_cnt = u16::try_from(arg_names.len())
			.map_err(|_| String::from("Too many arguments in function"))?;
		Ok(FunctionContext {
			func: CompiledFunction::new(arg_cnt),
			arg_names,
			used_regs: IntSet::new(),
			blocks: vec![],
			stack_size: 0,
		})
	}
	
	fn find_local(&self, id: &str) -> Option<u16> {
		self.blocks.iter().rev().find_map(|b| b.locals.get(id).copied())
	}
	
	fn compile_sequence(&mut self, values: Vec<Expr>, mutable: bool) -> Result<(), String> {
		let len = u16::try_from(values.len())
			.map_err(|_| format!("{} is too long", if mutable { "List" } else { "Tuple"} ))?;
		for expr in values {
			self.compile_expression(expr)?;
		}
		if mutable {
			self.func.code.push(Instr::NewList(len));
		} else {
			self.func.code.push(Instr::NewTuple(len));
		}
		self.stack_size -= len as usize;
		self.stack_size += 1;
		Ok(())
	}
	
	fn add_prim_cst(&mut self, cst: Primitive) -> Result<u16, String> {
		if let Some(idx) = self.func.csts.iter().position(|cst2| &cst == cst2) {
			Ok(idx as u16)
		} else {
			let idx = u16::try_from(self.func.csts.len())
				.map_err(|_| String::from("Too many constants"))?;
			self.func.csts.push(cst);
			Ok(idx)
		}
	}

	pub fn compile_expression(&mut self, expr: Expr) -> Result<(), String> {
		match expr {
			Expr::Primitive(cst) => {
				let idx = self.add_prim_cst(cst)?;
				self.func.code.push(Instr::Constant(idx));
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
				self.func.code.push(Instr::NewMap(len));
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
				let idx = self.func.classes.iter().position(|c| c == &class)
					.unwrap_or_else(|| {
						let idx = self.func.classes.len();
						self.func.classes.push(class);
						idx
					});
				let idx = u16::try_from(idx).map_err(|_| String::from("Too many object classes"))?;
				self.func.code.push(Instr::NewObject(idx));
				self.stack_size -= len as usize;
				self.stack_size += 1;
			},
			Expr::Function(arg_names, block) => {
				let idx = u16::try_from(self.func.child_funcs.len())
					.map_err(|_| String::from("Too many functions"))?;
				let func = FunctionContext::new(arg_names)?
					.compile_function(block)?;
				self.func.child_funcs.push(Rc::new(func));
				self.func.code.push(Instr::NewFunction(idx));
				self.stack_size += 1;
			},
			Expr::LExpr(lexpr) => {
				match lexpr {
					LExpr::Id(id) => {
						match self.find_local(&id) {
							Some(reg) => {
								self.func.code.push(Instr::Load(reg));
								self.stack_size += 1;
							},
							None => return Err(format!("Referencing undefined local '{}'", id)),
						}
					},
					LExpr::Index(seq, idx) => {
						self.compile_expression(*seq)?;
						self.compile_expression(*idx)?;
						self.func.code.push(Instr::Index);
						self.stack_size -= 1;
					},
					LExpr::Prop(obj, prop) => {
						self.compile_expression(*obj)?;
						let cst_idx = self.add_prim_cst(Primitive::String(prop))?;
						self.func.code.push(Instr::Prop(cst_idx));
					},
				}
			},
			Expr::Call(func, args) => {
				let arg_cnt = u16::try_from(args.len())
					.map_err(|_| String::from("Too many arguments in function call"))?;
				self.compile_expression(*func)?;
				for arg in args {
					self.compile_expression(arg)?;
				}
				self.func.code.push(Instr::Call(arg_cnt));
				self.stack_size -= arg_cnt as usize;
			},
			Expr::Unary(op, expr) => {
				self.compile_expression(*expr)?;
				self.func.code.push(Instr::Unary(op));
			},
			Expr::Binary(op, expr1, expr2) => {
				self.compile_expression(*expr1)?;
				self.compile_expression(*expr2)?;
				self.func.code.push(Instr::Binary(op));
				self.stack_size -= 1;
			},
			_ => { todo!() },
		}
		Ok(())
	}
	
	fn compute_jump_from(&self, from: usize) -> Result<i16, String> {
		i16::try_from((self.func.code.len() as isize) - (from as isize))
			.map_err(|_| String::from("Jump in code is too far to encode"))
	}
	fn compute_jump_to(&self, to: usize) -> Result<i16, String> {
		i16::try_from((to as isize) - (self.func.code.len() as isize))
			.map_err(|_| String::from("Jump in code is too far to encode"))
	}

	pub fn compile_statement(&mut self, stat: Statement) -> Result<(), String> {
		match stat {
			Statement::Let(lexpr, expr) => {
				match lexpr {
					LExpr::Id(id) => {
						let reg = {
							if let Some(reg) = self.blocks.last_mut().unwrap().locals.get(&id).copied() { // Shadowing previous binding
								self.func.code.push(Instr::Drop(reg));
								reg
							} else {
								let reg = self.used_regs.find_hole();
								self.used_regs.add(reg);
								self.blocks.last_mut().unwrap().locals.insert(id, reg);
								reg
							}
						};
						
						self.compile_expression(expr)?;
						
						self.func.code.push(Instr::Store(reg));
						self.stack_size -= 1;
					},
					_ => { todo!() },
				}
			},
			Statement::Set(lexpr, expr) => {
				self.compile_expression(expr)?;
				match lexpr {
					LExpr::Id(id) => {
						if let Some(reg) = self.find_local(&id) {
							self.func.code.push(Instr::Store(reg));
							self.stack_size -= 1;
						} else {
							return Err(format!("Assigning to undefined local '{}'", id));
						}
					},
					_ => { todo!() },
				}
			},
			Statement::If(cond, then, elseifs, else_block) => {
				let mut end_jumps = vec![];
				let mut prev_jump;
				
				for (cond, block) in vec![(cond, then)].drain(..).chain(elseifs) {
					self.compile_expression(cond)?;
					prev_jump = self.func.code.len();
					self.func.code.push(Instr::JumpIfNot(0));
					self.stack_size -= 1;
					self.compile_block(block, BlockType::Normal)?;
					end_jumps.push(self.func.code.len());
					self.func.code.push(Instr::Jump(0));
					self.func.code[prev_jump] = Instr::JumpIfNot(
						self.compute_jump_from(prev_jump)?);
				}
				
				if let Some(else_block) = else_block {
					self.compile_block(else_block, BlockType::Normal)?;
				}
				
				for end_jump in end_jumps {
					self.func.code[end_jump] = Instr::Jump(
						self.compute_jump_from(end_jump)?);
				}
			}
			Statement::Loop(block) => {
				let start = self.func.code.len();
				let breaks = self.compile_block(block, BlockType::Breakable)?;
				let jump = self.compute_jump_to(start)?;
				self.func.code.push(Instr::Jump(jump));
				for break_idx in breaks {
					self.func.code[break_idx] = Instr::Jump(
						self.compute_jump_from(break_idx)?);
				}
			},
			Statement::Break(loops) => {
				if let Some(block_idx) = self.blocks.iter().enumerate().rev()
						.filter(|(_,b)| b.breakable).nth(loops as usize - 1).map(|(i,_)| i) {
					// Drop locals from traversed blocks
					for block in self.blocks[block_idx..].iter().rev() {
						for (_, reg) in &block.locals {
							self.func.code.push(Instr::Drop(*reg));
						}
					}
					let block = &mut self.blocks[block_idx];
					block.breaks.push(self.func.code.len());
					self.func.code.push(Instr::Jump(0));
				} else {
					return Err(format!("Cannot break {} loops", loops));
				}
			},
			Statement::Return(expr) => {
				self.compile_expression(expr)?;
				self.func.code.push(Instr::Return);
				self.stack_size -= 1;
			}
			Statement::Log(expr) => { // Temporary
				self.compile_expression(expr)?;
				self.func.code.push(Instr::Log);
				self.stack_size -= 1;
			},
			Statement::ExprStat(expr) => {
				self.compile_expression(expr)?;
				self.func.code.push(Instr::Discard);
				self.stack_size -= 1;
			},
			_ => { todo!() }
		}
		
		assert!(self.stack_size == 0, "Compiler logic error: {} values remained on stack after statement compilation", self.stack_size);
		
		Ok(())
	}
	
	pub fn start_block(&mut self, block_type: BlockType) {
		self.blocks.push(BlockContext {
			breakable: block_type == BlockType::Breakable,
			breaks: vec![],
			locals: HashMap::new()
		});
		
		if block_type == BlockType::FunctionMain {
			for (idx, arg_name) in self.arg_names.iter().enumerate() {
				let reg = idx as u16;
				self.used_regs.add(reg);
				self.blocks.last_mut().unwrap().locals.insert(arg_name.clone(), reg);
			}
		}
	}
	
	// Returns a list of code indices with jumps that should jump out of loop,
	// in the case of a Breakable block
	fn compile_block(&mut self, block: Block, block_type: BlockType) -> Result<Vec<usize>, String> {
		self.start_block(block_type);
		
		for stat in block {
			self.compile_statement(stat)?;
		}
		
		let mut block = self.blocks.pop().unwrap();
		
		for (_, reg) in block.locals.drain() {
			self.func.code.push(Instr::Drop(reg));
			self.used_regs.remove(reg);
		}
		
		Ok(block.breaks)
	}
	
	pub fn add_log(&mut self) {
		self.func.code.push(Instr::Log);
		self.stack_size -= 1;
	}
	
	pub fn compile_function(mut self, block: Block) -> Result<CompiledFunction, String> {
		self.compile_block(block, BlockType::FunctionMain)?;
		
		Ok(self.func)
	}
}

pub fn compile_program(prog: Block) -> Result<CompiledFunction, String> {
	FunctionContext::new(vec![])?.compile_function(prog)
}
