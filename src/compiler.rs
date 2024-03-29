use std::convert::TryFrom;
use std::rc::Rc;

use fnv::FnvHashMap;

use crate::ast::*;
use crate::chunk::*;
use crate::util::IntSet;
use crate::value::Value;


#[derive(Clone)]
pub struct BlockContext {
	breakable: bool,
	breaks: Vec<usize>,
	continues: Vec<usize>,
	locals: FnvHashMap<String, u16>,
}

#[derive(PartialEq)]
pub enum BlockType {
	FunctionMain,
	Normal,
	Breakable,
}

#[derive(Clone)]
pub struct UpvalueSpec { up: usize, reg: u16 }

#[derive(Clone)]
pub struct FunctionContext<'a> {
	parent: Option<&'a FunctionContext<'a>>,
	pub func: CompiledFunction,
	arg_names: Vec<String>,
	used_regs: IntSet<u16>,
	blocks: Vec<BlockContext>,
	cur_def: Option<(String, u16)>,
	cur_code_pos: Option<usize>,
	upvalues: Vec<UpvalueSpec>,
	stack_size: usize,
}

pub struct InteractiveContext {
	used_regs: IntSet<u16>,
	locals: FnvHashMap<String, u16>,
}

impl InteractiveContext {
	pub fn new() -> Self {
		InteractiveContext {
			used_regs: IntSet::new(),
			locals: FnvHashMap::default(),
		}
	}
	
	pub fn make_func_ctx(&self) -> FunctionContext<'static> {
		FunctionContext {
			parent: None,
			func: CompiledFunction::new(0, Some(String::from("<repl>"))),
			arg_names: vec![],
			used_regs: self.used_regs.clone(),
			blocks: vec![
				BlockContext {
					breakable: false, breaks: vec![], continues: vec![],
					locals: self.locals.clone(),
				}
			],
			cur_def: None,
			cur_code_pos: None,
			upvalues: vec![],
			stack_size: 0,
		}
	}
	
	pub fn apply_func_ctx(&mut self, func_ctx: FunctionContext<'static>) {
		let FunctionContext { used_regs, mut blocks, .. } = func_ctx;
		self.used_regs = used_regs;
		self.locals = blocks.remove(0).locals;
	}
}

impl<'a> FunctionContext<'a> {
	pub fn new(arg_names: Vec<String>, parent: Option<&'a FunctionContext<'a>>, name: Option<String>) -> Result<Self, String> {
		let arg_cnt = u16::try_from(arg_names.len())
			.map_err(|_| String::from("Too many arguments in function"))?;
		Ok(FunctionContext {
			parent,
			func: CompiledFunction::new(arg_cnt, name),
			arg_names,
			used_regs: IntSet::new(),
			blocks: vec![],
			cur_def: None,
			cur_code_pos: None,
			upvalues: vec![],
			stack_size: 0,
		})
	}
	
	fn emit(&mut self, instr: Instr) {
		self.func.code.push(instr);
		self.func.code_pos.push(self.cur_code_pos.unwrap());
	}
	
	fn new_upvalue(&mut self, upv: UpvalueSpec) -> Result<u16, String> {
		let upv_idx = self.upvalues.len();
		let upv_idx = u16::try_from(upv_idx)
			.map_err(|_| String::from("Too many upvalues"))?;
		self.upvalues.push(upv);
		Ok(upv_idx)
	}
	
	// block.cur_def counts (allowing indirect recursive definitions)
	fn find_upvalue(&mut self, id: &str) -> Result<Option<u16>, String> {
		if let Some(mut ctx) = self.parent {
			let mut up = 1;
			loop {
				let reg = ctx.cur_def.as_ref()
					.filter(|(id2,_)| id == id2)
					.map(|(_,reg)| *reg)
					.or_else(|| ctx.find_local(id));
				if let Some(reg) = reg {
					let upv = self.new_upvalue(UpvalueSpec { up, reg })?;
					return Ok(Some(upv));
				}
				if let Some(ctx2) = ctx.parent {
					ctx = ctx2;
					up += 1;
				} else {
					return Ok(None)
				}
			}
		} else {
			Ok(None)
		}
	}
	
	fn compile_upvalues(&mut self, func: &mut CompiledFunction, upvalues: Vec<UpvalueSpec>) -> Result<(), String> {
		for upv in upvalues {
			let UpvalueSpec { up, reg } = upv;
			if up == 1 {
				func.upvalues.push(CompiledUpvalue::Direct(reg));
			} else {
				let upv = self.new_upvalue(UpvalueSpec { up: up - 1, reg })?;
				func.upvalues.push(CompiledUpvalue::Indirect(upv));
			}
		}
		Ok(())
	}
	
	// block.cur_def does not count (no direct recursive definitions)
	fn find_local(&self, id: &str) -> Option<u16> {
		self.blocks.iter().rev().find_map(|b| b.locals.get(id).copied())
	}
	
	fn compile_sequence(&mut self, values: Vec<Expr>, mutable: bool) -> Result<(), String> {
		let len = u16::try_from(values.len())
			.map_err(|_| format!("{} is too long", if mutable { "List" } else { "Tuple"} ))?;
		for expr in values {
			self.compile_expression(expr, None)?;
		}
		if mutable {
			self.emit(Instr::NewList(len));
		} else {
			self.emit(Instr::NewTuple(len));
		}
		self.stack_size -= len as usize;
		Ok(())
	}
	
	fn add_prim_cst(&mut self, cst: Value) -> Result<u16, String> {
		if let Some(idx) = self.func.csts.iter().position(|cst2| &cst == cst2) {
			Ok(idx as u16)
		} else {
			let idx = u16::try_from(self.func.csts.len())
				.map_err(|_| String::from("Too many constants"))?;
			self.func.csts.push(cst);
			Ok(idx)
		}
	}

	pub fn compile_expression(&mut self, expr: Expr, name: Option<String>) -> Result<(), String> {
		match expr {
			Expr::Primitive(cst) => {
				let idx = self.add_prim_cst(Value::from(&cst))?;
				self.emit(Instr::Constant(idx));
			},
			Expr::Tuple(values) => self.compile_sequence(values, false)?,
			Expr::List(values) => self.compile_sequence(values, true)?,
			Expr::Map(pairs) => {
				let len = u16::try_from(pairs.len())
					.map_err(|_| String::from("Map is too large"))?;
				for (key, value) in pairs {
					self.compile_expression(key, None)?;
					self.compile_expression(value, None)?;
				}
				self.emit(Instr::NewMap(len));
				self.stack_size -= 2*len as usize;
			},
			Expr::Struct(pairs) => {
				let len = u16::try_from(pairs.len())
					.map_err(|_| String::from("Struct is too large"))?;
				let mut class = vec![];
				for (id, value) in pairs {
					class.push(id.clone());
					self.compile_expression(value, Some(id))?;
				}
				let idx = self.func.classes.iter().position(|c| c == &class)
					.unwrap_or_else(|| {
						let idx = self.func.classes.len();
						self.func.classes.push(class);
						idx
					});
				let idx = u16::try_from(idx).map_err(|_| String::from("Too many struct classes"))?;
				self.emit(Instr::NewStruct(idx));
				self.stack_size -= len as usize;
			},
			Expr::Function(arg_names, block) => {
				let idx = u16::try_from(self.func.child_funcs.len())
					.map_err(|_| String::from("Too many functions"))?;
				let (mut func, upvalues) = FunctionContext::new(arg_names, Some(self), name)?
					.compile_function(block)?;
				self.compile_upvalues(&mut func, upvalues)?;
				self.func.child_funcs.push(Rc::new(func));
				self.emit(Instr::NewFunction(idx));
			},
			Expr::LExpr(lexpr) => {
				match lexpr {
					LExpr::Id(id) => {
						if let Some(reg) = self.find_local(&id) {
							self.emit(Instr::Load(reg));
						} else if let Some(upv) = self.find_upvalue(&id)? {
							self.emit(Instr::LoadUpv(upv));
						} else if crate::stdlib::GLOBAL_NAMES.contains(&id) {
							let idx = self.add_prim_cst(Value::String(id.into_boxed_str()))?;
							self.emit(Instr::LoadGlobal(idx));
						} else {
							return Err(format!("Referencing undefined value '{}'", id));
						}
					},
					LExpr::Index(seq, idx) => {
						self.compile_expression(*seq, None)?;
						self.compile_expression(*idx, None)?;
						self.emit(Instr::Index);
						self.stack_size -= 2;
					},
					LExpr::Prop(obj, prop) => {
						self.compile_expression(*obj, None)?;
						let cst_idx = self.add_prim_cst(Value::String(prop.into_boxed_str()))?;
						self.emit(Instr::Prop(cst_idx));
						self.stack_size -= 1;
					},
				}
			},
			Expr::Call(func, args) => {
				let arg_cnt = u16::try_from(args.len())
					.map_err(|_| String::from("Too many arguments in function call"))?;
				self.compile_expression(*func, None)?;
				for arg in args {
					self.compile_expression(arg, None)?;
				}
				self.emit(Instr::Call(arg_cnt));
				self.stack_size -= 1 + (arg_cnt as usize);
			},
			Expr::Unary(op, expr) => {
				self.compile_expression(*expr, None)?;
				self.emit(Instr::Unary(op));
				self.stack_size -= 1;
			},
			Expr::Binary(op, expr1, expr2) => {
				self.compile_expression(*expr1, None)?;
				let jump = self.func.code.len();
				match op {
					BinaryOp::Or => {
						self.emit(Instr::JumpOr(0));
						self.compile_expression(*expr2, None)?;
						self.func.code[jump] = Instr::JumpOr(
							self.compute_jump_from(jump)?);
					},
					BinaryOp::And => {
						self.emit(Instr::JumpAnd(0));
						self.compile_expression(*expr2, None)?;
						self.func.code[jump] = Instr::JumpAnd(
							self.compute_jump_from(jump)?);
					},
					_ => {
						self.compile_expression(*expr2, None)?;
						self.emit(Instr::Binary(op));
					}
				}
				self.stack_size -= 2;
			},
			Expr::Condition(cond, expr1, expr2) => {
				self.compile_expression(*cond, None)?;
				let jump1 = self.func.code.len();
				self.emit(Instr::JumpIfNot(0));
				self.stack_size -= 1;
				self.compile_expression(*expr1, None)?;
				let jump2 = self.func.code.len();
				self.emit(Instr::Jump(0));
				self.func.code[jump1] = Instr::JumpIfNot(
					self.compute_jump_from(jump1)?);
				self.compile_expression(*expr2, None)?;
				self.func.code[jump2] = Instr::Jump(
					self.compute_jump_from(jump2)?);
				self.stack_size -= 2;
			},
		}
		self.stack_size += 1;
		Ok(())
	}
	
	fn compute_jump(&self, from: usize, to: usize) -> Result<i16, String> {
		i16::try_from((to as isize) - (from as isize))
			.map_err(|_| String::from("Jump in code is too far to encode"))
	}
	fn compute_jump_from(&self, from: usize) -> Result<i16, String> {
		self.compute_jump(from, self.func.code.len())
	}
	fn compute_jump_to(&self, to: usize) -> Result<i16, String> {
		self.compute_jump(self.func.code.len(), to)
	}
	
	fn new_reg(&mut self) -> u16 {
		let reg = self.used_regs.find_hole();
		self.used_regs.add(reg);
		reg
	}
	
	fn start_def(&mut self, id: &str) {
		let reg = self.blocks.last_mut().unwrap().locals.get(id).copied()
			.unwrap_or_else(|| self.new_reg());
		self.cur_def = Some((id.to_string(), reg));
	}
	fn finish_def(&mut self) -> u16 {
		let (id, reg) = self.cur_def.take().unwrap();
		let locals = &mut self.blocks.last_mut().unwrap().locals;
		if locals.contains_key(&id) {
			// Shadow previous binding
			self.emit(Instr::Drop(reg));
		} else {
			locals.insert(id, reg);
		}
		self.emit(Instr::Store(reg));
		self.stack_size -= 1;
		reg
	}

	pub fn compile_statement(&mut self, stat: PositionedStatement) -> Result<(), String> {
		let PositionedStatement(pos, stat) = stat;
		self.cur_code_pos = Some(pos);
		match stat {
			Statement::Let(pat, expr) => {
				match pat {
					Pattern::Id(id) => {
						self.start_def(&id);
						
						self.compile_expression(expr, Some(id))?;
						
						self.finish_def();
					},
				}
			},
			Statement::Set(lexpr, expr) => {
				match lexpr {
					LExpr::Id(id) => {
						if let Some(reg) = self.find_local(&id) {
							self.compile_expression(expr, Some(id))?;
							self.emit(Instr::Store(reg));
							self.stack_size -= 1;
						} else if let Some(upv) = self.find_upvalue(&id)? {
							self.compile_expression(expr, Some(id))?;
							self.emit(Instr::StoreUpv(upv));
							self.stack_size -= 1;
						} else {
							return Err(format!("Referencing undefined local '{}'", id));
						}
					},
					LExpr::Index(coll, idx) => {
						self.compile_expression(*coll, None)?;
						self.compile_expression(*idx, None)?;
						self.compile_expression(expr, None)?;
						self.emit(Instr::SetIndex);
						self.stack_size -= 3;
					},
					LExpr::Prop(obj, prop) => {
						self.compile_expression(*obj, None)?;
						let cst_idx = self.add_prim_cst(Value::String(prop.clone().into_boxed_str()))?;
						self.compile_expression(expr, Some(prop))?;
						self.emit(Instr::SetProp(cst_idx));
						self.stack_size -= 2;
					},
				}
			},
			Statement::If(cond, then, elseifs, else_block) => {
				let mut end_jumps = vec![];
				let mut prev_jump;
				
				let conds = 1 + elseifs.len();
				
				for (i, (cond, block)) in vec![(cond, then)].drain(..).chain(elseifs).enumerate() {
					self.compile_expression(cond, None)?;
					prev_jump = self.func.code.len();
					self.emit(Instr::JumpIfNot(0));
					self.stack_size -= 1;
					
					self.compile_block(block, BlockType::Normal)?;
					
					// Only place a jump if there is another block afterwars
					if !(i == conds - 1 && else_block.is_none()) {
						end_jumps.push(self.func.code.len());
						self.emit(Instr::Jump(0));
					}
					
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
			},
			Statement::While(cond, block) => {
				let start = self.func.code.len();
				
				self.compile_expression(cond, None)?;
				let end_jump = self.func.code.len();
				self.emit(Instr::JumpIfNot(0));
				self.stack_size -= 1;
				
				let block_ctx = self.compile_block(block, BlockType::Breakable)?;
				self.emit(Instr::Jump(
					self.compute_jump_to(start)?));
				
				self.func.code[end_jump] = Instr::JumpIfNot(
					self.compute_jump_from(end_jump)?);
				
				self.finish_loop(block_ctx, start)?;
			},
			Statement::DoWhile(block, cond) => {
				let start = self.func.code.len();
				
				let block_ctx = self.compile_block(block, BlockType::Breakable)?;
				
				let cond_idx = self.func.code.len();
				self.compile_expression(cond, None)?;
				self.emit(Instr::JumpIf(
					self.compute_jump_to(start)?));
				self.stack_size -= 1;
				
				self.finish_loop(block_ctx, cond_idx)?;
			},
			Statement::Loop(block) => {
				let start = self.func.code.len();
				
				let block_ctx = self.compile_block(block, BlockType::Breakable)?;
				
				self.emit(Instr::Jump(self.compute_jump_to(start)?));
				
				self.finish_loop(block_ctx, start)?;
			},
			Statement::For(id, iter, block) => {
				// Keep track of iterator in anonymous local in block around
				// loop block
				self.start_block(BlockType::Normal);
				self.start_def("");
				self.compile_expression(iter, None)?;
				let iter_reg = self.finish_def();
				
				let start = self.func.code.len();
				
				self.start_block(BlockType::Breakable);
				
				self.emit(Instr::Next(iter_reg));
				self.stack_size += 1;
				let end_jump = self.func.code.len();
				self.emit(Instr::JumpIfNot(0));
				self.start_def(&id);
				self.finish_def();
				
				for stat in block {
					self.compile_statement(stat)?;
				}
				
				let block_ctx = self.finish_block();
				
				self.emit(Instr::Jump(
					self.compute_jump_to(start)?));
				self.finish_loop(block_ctx, start)?;
				
				self.func.code[end_jump] = Instr::JumpIfNot(
					self.compute_jump_from(end_jump)?);
				
				self.finish_block();
			},
			Statement::Break(loops) => {
				if let Some(block_idx) = self.blocks.iter().enumerate().rev()
						.filter(|(_,b)| b.breakable).nth(loops as usize - 1).map(|(i,_)| i) {
					// Drop locals from traversed blocks
					for block_idx in (block_idx..self.blocks.len()).rev() {
						self.drop_locals(block_idx, true);
					}
					let block = &mut self.blocks[block_idx];
					block.breaks.push(self.func.code.len());
					self.emit(Instr::Jump(0));
				} else {
					return Err(format!("Cannot break {} loops", loops));
				}
			},
			Statement::Continue(loops) => {
				if let Some(block_idx) = self.blocks.iter().enumerate().rev()
						.filter(|(_,b)| b.breakable).nth(loops as usize - 1).map(|(i,_)| i) {
					// Drop locals from traversed blocks
					for block_idx in (block_idx..self.blocks.len()).rev() {
						self.drop_locals(block_idx, true);
					}
					let block = &mut self.blocks[block_idx];
					block.continues.push(self.func.code.len());
					self.emit(Instr::Jump(0));
				} else {
					return Err(format!("Cannot continue {} loops", loops));
				}
			},
			Statement::Return(expr) => {
				self.compile_expression(expr, None)?;
				// Drop all locals
				for block_idx in (0..self.blocks.len()).rev() {
					self.drop_locals(block_idx, true);
				}
				self.emit(Instr::Return);
				self.stack_size -= 1;
			},
			Statement::ExprStat(expr) => {
				self.compile_expression(expr, None)?;
				self.emit(Instr::Discard);
				self.stack_size -= 1;
			},
		}
		
		assert!(self.stack_size == 0, "Compiler logic error: {} values remained on stack after statement compilation", self.stack_size);
		
		Ok(())
	}
	
	pub fn start_block(&mut self, block_type: BlockType) {
		self.blocks.push(BlockContext {
			breakable: block_type == BlockType::Breakable,
			breaks: vec![],
			continues: vec![],
			locals: FnvHashMap::default()
		});
		
		if block_type == BlockType::FunctionMain {
			for (idx, arg_name) in self.arg_names.iter().enumerate() {
				let reg = idx as u16;
				self.used_regs.add(reg);
				self.blocks.last_mut().unwrap().locals.insert(arg_name.clone(), reg);
			}
		}
	}
	
	// emit_only should be true for instructions that drop locals
	// before jumping, where we don't want to forget about those
	// locals in the rest of the code quite yet.
	fn drop_locals(&mut self, block_idx: usize, emit_only: bool) {
		let regs: Vec<u16> = self.blocks[block_idx].locals.values().copied().collect();
		for reg in regs {
			self.emit(Instr::Drop(reg));
			if !emit_only { self.used_regs.remove(reg); }
		}
	}
	
	fn finish_block(&mut self) -> BlockContext {
		self.drop_locals(self.blocks.len() - 1, false);
		self.blocks.pop().unwrap()
	}
	
	// Returns a list of code indices with jumps that should jump out of loop,
	// in the case of a Breakable block
	fn compile_block(&mut self, block: Block, block_type: BlockType) -> Result<BlockContext, String> {
		self.start_block(block_type);
		
		for stat in block {
			self.compile_statement(stat)?;
		}
		
		Ok(self.finish_block())
	}
	
	fn finish_loop(&mut self, block: BlockContext, in_idx: usize) -> Result<(), String> {
		for break_idx in block.breaks {
			self.func.code[break_idx] = Instr::Jump(
				self.compute_jump_from(break_idx)?);
		}
		
		for continue_idx in block.continues {
			self.func.code[continue_idx] = Instr::Jump(
				self.compute_jump(continue_idx, in_idx)?);
		}
		
		Ok(())
	}
	
	pub fn compile_expression_log(&mut self, expr: Expr) -> Result<(), String> {
		self.cur_code_pos = Some(1);
		self.compile_expression(expr, None)?;
		self.emit(Instr::Log);
		self.stack_size -= 1;
		Ok(())
	}
	
	pub fn compile_function(mut self, block: Block) -> Result<(CompiledFunction, Vec<UpvalueSpec>), String> {
		self.compile_block(block, BlockType::FunctionMain)?;
		Ok((self.func, self.upvalues))
	}
}

pub fn compile_program(prog: Block) -> Result<CompiledFunction, String> {
	Ok(FunctionContext::new(vec![], None, Some(String::from("<root>")))?.compile_function(prog)?.0)
}
