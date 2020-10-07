use std::collections::HashMap;
use std::cmp::Ordering;
use std::convert::TryFrom;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use gc_arena::{Collect, Gc, GcCell, MutationContext};

use crate::ast::{UnaryOp, BinaryOp};
use crate::chunk::*;
use crate::value::*;

#[derive(Collect)]
#[collect(no_drop)]
struct Call<'gc> {
	func: Gc<'gc, Function<'gc>>,
	interactive: bool,
	reg_base: usize,
	return_idx: Option<usize>,
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct VmState<'gc> {
	globals: HashMap<String, Value<'gc>>,
	calls: Vec<Call<'gc>>,
	registers: Vec<Value<'gc>>,
	stack: Vec<Value<'gc>>,
	upvalues: HashMap<usize, GcCell<'gc, Upvalue<'gc>>>,
	reg_base: usize,
	idx: usize,
}

impl<'gc> VmState<'gc> {
	pub fn new() -> Self {
		VmState {
			globals: HashMap::new(),
			calls: vec![],
			registers: vec![],
			stack: vec![],
			upvalues: HashMap::new(),
			reg_base: 0,
			idx: 0,
		}
	}
	
	fn init_globals(&mut self, mc: MutationContext<'gc, '_>) {
		crate::stdlib::create(&mut self.globals, mc);
	}
	
	fn set_reg(&mut self, reg: u16, val: Value<'gc>) {
		let reg = self.reg_base + reg as usize;
		if reg >= self.registers.len() {
			self.registers.resize(reg + 1, Value::Nil);
		}
		self.registers[reg] = val;
	}
	
	fn get_raw_reg(&self, reg: usize) -> Result<&Value<'gc>, String> {
		self.registers.get(reg)
			.ok_or_else(|| String::from("Accessing uninitialized register"))
	}
	
	fn get_reg(&self, reg: u16) -> Result<&Value<'gc>, String> {
		let reg = self.reg_base + reg as usize;
		self.get_raw_reg(reg)
	}
	
	fn close_reg_raw(&mut self, mc: MutationContext<'gc, '_>, reg: usize) {
		if let Some(cell) = self.upvalues.remove(&reg) {
			let val = self.registers[reg].clone();
			*cell.write(mc) = Upvalue::Closed(val);
		}
	}
	
	fn drop_reg(&mut self, mc: MutationContext<'gc, '_>, reg: u16) {
		let reg = self.reg_base + reg as usize;
		self.close_reg_raw(mc, reg);
		
		if reg == self.registers.len() - 1 {
			self.registers.pop();
		} else {
			self.registers[reg] = Value::Nil;
		}
	}
	
	fn get_upv(&self, upv: &Upvalue<'gc>) -> Result<Value<'gc>, String> {
		match upv {
			Upvalue::Open(reg) => Ok(self.get_raw_reg(*reg as usize)?.clone()),
			Upvalue::Closed(val) => Ok(val.clone()),
		}
	}
	
	fn make_upv(&mut self, mc: MutationContext<'gc, '_>, reg: u16) -> GcCell<'gc, Upvalue<'gc>> {
		let reg = self.reg_base + reg as usize;
		if let Some(upv) = self.upvalues.get(&reg) {
			*upv
		} else {
			let upv = GcCell::allocate(mc, Upvalue::Open(reg));
			self.upvalues.insert(reg, upv);
			upv
		}
	}
	
	pub fn get_global(&self, name: &str) -> Result<Value<'gc>, String> {
		self.globals.get(name).cloned()
			.ok_or_else(|| format!("Accessing undefined global '{}'", name))
	}
	
	pub fn push(&mut self, val: Value<'gc>) {
		self.stack.push(val);
	}
	
	pub fn pop(&mut self) -> Result<Value<'gc>, String> {
		self.stack.pop()
			.ok_or_else(|| String::from("Popping from empty stack"))
	}
	
	fn pop_n(&mut self, n: usize) -> Result<Vec<Value<'gc>>, String> {
		let start = self.stack.len().checked_sub(n)
			.ok_or_else(|| String::from("Popping too many values from stack"))?;
		Ok(self.stack.drain(start..).collect())
	}
	
	fn get_cst(&self, func: &CompiledFunction, idx: u16) -> Result<Value<'gc>, String> {
		let cst = func.csts.get(idx as usize)
			.ok_or_else(|| String::from("Accessing undefined constant"))?;
		Ok(cst.clone_prim())
	}
	
	fn jump(&mut self, rel: i16) {
		if rel <= 0 {
			self.idx -= -rel as usize;
		} else {
			self.idx += rel as usize;
		}
	}
	
	fn call_function(&mut self, func: Gc<'gc, Function<'gc>>, mut args: Vec<Value<'gc>>, no_return: bool) {
		if !no_return {
			let last = self.calls.last_mut().unwrap();
			last.return_idx = Some(self.idx + 1);
		}
		let reg_base = self.registers.len();
		self.reg_base = reg_base;
		self.calls.push(Call {
			func,
			interactive: false,
			reg_base,
			return_idx: None,
		});
		
		for (i, arg) in args.drain(..).enumerate() {
			self.set_reg(i as u16, arg);
		}
		
		self.idx = 0;
	}
	
	// Runs function at top level in "interactive mode"
	// This means that the function's registers won't be dropped after execution,
	// and some more code within the same scope can be run afterwards.
	// Used in REPL.
	fn call_interactive(&mut self, mc: MutationContext<'gc, '_>, func: Rc<CompiledFunction>) {
		assert!(self.calls.len() == 0);
		
		self.calls.push(Call {
			func: Gc::allocate(mc, Function::main(func)),
			interactive: true,
			reg_base: 0,
			return_idx: None,
		});
		
		self.idx = 0;
	}
	
	fn return_from_call(&mut self, mc: MutationContext<'gc, '_>, implicit: bool) {
		let call = self.calls.pop().unwrap();
		if !call.interactive { // If in interactive mode, keep rest of state for next call
			if implicit {
				self.push(Value::Nil)
			}
			for reg in call.reg_base .. self.registers.len() {
				self.close_reg_raw(mc, reg);
			}
			self.registers.truncate(call.reg_base);
		}
		
		if let Some(call) = self.calls.last_mut() {
			if let Some(idx) = call.return_idx.take() {
				self.idx = idx;
			}
			self.reg_base = call.reg_base;
		}
	}
	
	pub fn check_end(&mut self, mc: MutationContext<'gc, '_>, base_call: usize) -> bool {
		// Implicit return
		while self.calls.len() > base_call && self.idx == self.calls.last().unwrap().func.chunk.code.len() {
			self.return_from_call(mc, true);
		}
		self.calls.len() == base_call
	}
	
	// Warning: does not reset state on errors; callers should not reuse state after error
	fn run_instr_unsafe(&mut self, mc: MutationContext<'gc, '_>) -> Result<(), String> {
		let func = self.calls.last().unwrap().func;
		let instr = func.chunk.code.get(self.idx)
			.ok_or_else(|| String::from("Jumped to invalid instruction"))?
			.clone();
		
		let mut jumped = false;
		
		match instr {
			Instr::Load(reg) => {
				let val = self.get_reg(reg)?.clone();
				self.push(val);
			},
			Instr::LoadUpv(upv_idx) => {
				let upv = func.upvalues.get(upv_idx as usize)
					.ok_or_else(|| String::from("Invalid upvalue index"))?;
				self.push(self.get_upv(&upv.read())?);
			},
			Instr::LoadGlobal(name_idx) => {
				let name = self.get_cst(&func.chunk, name_idx)?.get_string()?;
				let val = self.get_global(&name)?;
				self.push(val);
			},
			Instr::Store(reg) => {
				let val = self.pop()?;
				self.set_reg(reg, val);
			},
			Instr::StoreUpv(upv_idx) => {
				let upv = func.upvalues.get(upv_idx as usize)
					.ok_or_else(|| String::from("Invalid upvalue index"))?;
				match upv.write(mc).deref_mut() {
					Upvalue::Open(reg) => {
						let val = self.pop()?;
						let slot = self.registers.get_mut(*reg)
							.ok_or_else(|| String::from("Upvalue points to invalid register"))?;
						*slot = val;
					},
					Upvalue::Closed(val) => {
						*val = self.pop()?;
					},
				};
			},
			Instr::Drop(reg) => {
				self.drop_reg(mc, reg);
			},
			Instr::Discard => {
				self.pop()?;
			},
			Instr::Log => {
				println!("{}", self.pop()?.repr());
			},
			Instr::Jump(rel) => {
				self.jump(rel);
				jumped = true;
			},
			Instr::JumpIf(rel) => {
				let val = self.pop()?;
				if val.get_bool()? {
					self.jump(rel);
					jumped = true;
				}
			},
			Instr::JumpIfNot(rel) => {
				let val = self.pop()?;
				if !val.get_bool()? {
					self.jump(rel);
					jumped = true;
				}
			},
			Instr::NewFunction(func_idx) => {
				let chunk2 = func.chunk.child_funcs.get(func_idx as usize)
					.ok_or_else(|| String::from("Accessing undefined function"))?
					.clone();
				let mut upvalues = vec![];
				for upv in &chunk2.upvalues {
					match upv {
						CompiledUpvalue::Direct(reg) => {
							upvalues.push(self.make_upv(mc, *reg));
						},
						CompiledUpvalue::Indirect(upv_idx) => {
							let upv = func.upvalues.get(*upv_idx as usize).copied()
								.ok_or_else(|| String::from("Child function references undefined upvalue"))?;
							upvalues.push(upv);
						},
					}
				}
				let func2 = Gc::allocate(mc, Function { chunk: chunk2, upvalues });
				self.push(Value::Function(func2));
			},
			Instr::Call(arg_cnt) => {
				let args: Vec<Value> = self.pop_n(arg_cnt as usize)?;
				let func2 = self.pop()?;
				match func2 {
					Value::Function(func2) => {
						let chunk2 = &func2.chunk;
						if args.len() != chunk2.arg_cnt as usize {
							return Err(format!("Expected {} arguments, got {}", chunk2.arg_cnt, args.len()));
						}
						self.call_function(func2, args, false);
						jumped = true;
					},
					Value::NativeFunction(func2) => {
						self.push(func2.write(mc).call(mc, args)?);
					},
					_ => {
						return Err(format!("Cannot call non-function: {}", func2.repr()));
					}
				}
			},
			Instr::Return => {
				self.return_from_call(mc, false);
				jumped = true;
			},
			Instr::Constant(idx) => {
				self.push(self.get_cst(&func.chunk, idx)?);
			},
			Instr::NewTuple(cnt) => {
				let values = self.pop_n(cnt as usize)?;
				self.push(Value::Tuple(Gc::allocate(mc, values)));
			},
			Instr::NewList(cnt) => {
				let values = self.pop_n(cnt as usize)?;
				self.push(Value::List(GcCell::allocate(mc, values)));
			},
			Instr::NewMap(cnt) => {
				let mut values = self.pop_n(2 * (cnt as usize))?;
				let (mut keys, mut values): (Vec<(usize,Value)>,Vec<(usize,Value)>) =
					values.drain(..).enumerate().partition(|(i,_)| i % 2 == 0);
				let map: HashMap<Value, Value> = keys.drain(..).map(|(_,v)| v).zip(values.drain(..).map(|(_,v)| v)).collect();
				self.push(Value::Map(GcCell::allocate(mc, map)));
			},
			Instr::NewObject(class_idx) => {
				let class = func.chunk.classes.get(class_idx as usize).ok_or_else(|| String::from("Using undefined object class"))?;
				let mut values = self.pop_n(class.len())?;
				let obj: HashMap<String, Value> = class.iter().cloned()
					.zip(values.drain(..)).collect();
				self.push(Value::Object(GcCell::allocate(mc, obj)));
			},
			Instr::Binary(op) => {
				let b = self.pop()?;
				let a = self.pop()?;
				match op {
					BinaryOp::And | BinaryOp::Or => {
						let a = a.get_bool()?;
						let b = b.get_bool()?;
						let c = match op {
							BinaryOp::And => a && b,
							BinaryOp::Or => a || b,
							_ => unreachable!(),
						};
						self.push(Value::Bool(c));
					},
					BinaryOp::Plus if a.get_type() == Type::String => {
						let a = a.get_string().unwrap();
						let b = b.get_string()?;
						self.push(Value::String(NiceStr((a + &b).into_boxed_str())));
					},
					BinaryOp::Plus if a.get_type() == Type::List => {
						let a = a.get_list().unwrap();
						let mut a = a.read().deref().clone();
						let b = b.get_list()?;
						a.extend_from_slice(b.read().deref());
						self.push(Value::List(GcCell::allocate(mc, a)));
					},
					BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Times | BinaryOp::Modulo
							if a.get_type() == Type::Int && b.get_type() == Type::Int => {
						let a = a.get_int().unwrap();
						let b = b.get_int().unwrap();
						let c = match op {
							BinaryOp::Plus => a.checked_add(b).ok_or_else(|| String::from("Integer overflow"))?,
							BinaryOp::Minus => a.checked_sub(b).ok_or_else(|| String::from("Integer overflow"))?,
							BinaryOp::Times => a.checked_mul(b).ok_or_else(|| String::from("Integer overflow"))?,
							BinaryOp::Modulo => a.checked_rem_euclid(b).ok_or_else(|| String::from("Division by zero"))?,
							_ => unreachable!(),
						};
						self.push(Value::Int(c));
					},
					BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Times | BinaryOp::Divides
					| BinaryOp::IntDivides | BinaryOp::Modulo | BinaryOp::Power => {
						let a = a.get_numeric().map_err(|err|
							if op == BinaryOp::Plus { expected_types::<()>("Int, Float, or String", &a).unwrap_err() } else { err })?;
						let b = b.get_numeric()?;
						let c = match op {
							BinaryOp::Plus => Value::try_from(a + b)?,
							BinaryOp::Minus => Value::try_from(a - b)?,
							BinaryOp::Times => Value::try_from(a * b)?,
							BinaryOp::Divides => {
								let c = a / b;
								Value::try_from(c)?
							},
							BinaryOp::IntDivides => {
								if b == 0.0 {
									return Err(String::from("Division by zero"));
								}
								let c = a.div_euclid(b);
								if c < (i32::MIN as f64) || c > (i32::MAX as f64) {
									return Err(String::from("Result of division does not fit in integer"));
								}
								Value::Int(c as i32)
							},
							BinaryOp::Modulo => Value::try_from(a.rem_euclid(b))?,
							BinaryOp::Power => Value::try_from(a.powf(b))?,
							_ => unreachable!(),
						};
						self.push(c);
					},
					BinaryOp::Eq => {
						self.push(Value::Bool(a.struct_eq(&b)));
					},
					BinaryOp::NotEq => {
						self.push(Value::Bool(!a.struct_eq(&b)));
					},
					BinaryOp::LessEq => {
						self.push(Value::Bool(a.cmp(&b)? <= Ordering::Equal));
					},
					BinaryOp::Less => {
						self.push(Value::Bool(a.cmp(&b)? < Ordering::Equal));
					},
					BinaryOp::Greater => {
						self.push(Value::Bool(a.cmp(&b)? > Ordering::Equal));
					},
					BinaryOp::GreaterEq => {
						self.push(Value::Bool(a.cmp(&b)? >= Ordering::Equal));
					},
				}
			},
			Instr::Unary(op) => {
				let val = self.pop()?;
				let res;
				match op {
					UnaryOp::Minus => {
						res = match val {
							Value::Int(i) => Value::Int(-i),
							Value::Float(f) => Value::Float(NiceFloat(-f.0)),
							_ => return Err(format!("Cannot get opposite of: {}", val.repr())),
						}
					},
					UnaryOp::Not => {
						if let Value::Bool(b) = val {
							res = Value::Bool(!b);
						} else {
							return Err(format!("Cannot get logical negation of: {}", val.repr()))
						}
					},
				}
				self.push(res);
			},
			Instr::Index => {
				let idx = self.pop()?;
				let coll = self.pop()?;
				self.push(coll.index(&idx)?);
			},
			Instr::SetIndex => {
				let val = self.pop()?;
				let idx = self.pop()?;
				let coll = self.pop()?;
				coll.set_index(&idx, val, mc)?;
			},
			Instr::Prop(cst_idx) => {
				let obj = self.pop()?;
				let idx = self.get_cst(&func.chunk, cst_idx)?.get_string()?;
				self.push(obj.prop(&idx, mc)?);
			},
			Instr::SetProp(cst_idx) => {
				let val = self.pop()?;
				let obj = self.pop()?;
				let idx = self.get_cst(&func.chunk, cst_idx)?.get_string()?;
				obj.set_prop(&idx, val, mc)?;
			},
			Instr::Next(iter_reg) => {
				let mut iter = self.get_reg(iter_reg)?;
				if let Some(new_iter) = iter.make_iter(mc)? {
					self.set_reg(iter_reg, new_iter);
					iter = self.get_reg(iter_reg)?;
				}
				if let Some(value) = iter.next(mc)? {
					self.push(value);
					self.push(Value::Bool(true));
				} else {
					self.push(Value::Bool(false));
				}
			},
		}
		
		if !jumped {
			self.idx += 1;
		}
		
		Ok(())
	}
	
	// Like run_instr_unsafe, but clears calls and the stack on errors
	// Does not clear registers
	// Using this, VmState can be reused in interactive mode even after error
	fn run_instr(&mut self, mc: MutationContext<'gc, '_>) -> Result<(), String> {
		match self.run_instr_unsafe(mc) {
			Ok(()) => Ok(()),
			Err(err) => {
				self.calls.clear();
				self.stack.clear();
				Err(err)
			}
		}
	}
}


type MutVmState<'gc> = GcCell<'gc, VmState<'gc>>;

make_arena!(pub VmArena, MutVmState);

impl VmArena {
	fn create() -> Self {
		let mut arena = VmArena::new(gc_arena::ArenaParameters::default(),
			|mc| GcCell::allocate(mc, VmState::new()));
		arena.mutate(|mc, state| state.write(mc).init_globals(mc));
		arena
	}
	
	fn run(&mut self, call_base: usize) -> Result<(), String> {
		loop {
			if self.mutate(|mc, state| state.write(mc).check_end(mc, call_base)) { break }
			self.mutate(|mc, state| state.write(mc).run_instr(mc))?
		}
		Ok(())
	}
	
	pub fn run_function(&mut self, func: Rc<CompiledFunction>) -> Result<(), String>  {
		let call_base = self.root.read().calls.len();
		self.mutate(|mc, state| {
			state.write(mc).call_function(Gc::allocate(mc, Function::main(func)), vec![], true);
		});
		self.run(call_base)
	}
	
	pub fn run_interactive(&mut self, func: Rc<CompiledFunction>) -> Result<(), String>  {
		let call_base = self.root.read().calls.len();
		self.mutate(|mc, state| {
			state.write(mc).call_interactive(mc, func);
		});
		self.run(call_base)
	}
}

pub struct InteractiveVm {
	arena: VmArena,
}

impl InteractiveVm {
	pub fn new() -> Self {
		InteractiveVm { arena: VmArena::create() }
	}
	
	pub fn execute_function(&mut self, func: Rc<CompiledFunction>) -> Result<(), String>  {
		self.arena.run_interactive(func)
	}
}

pub fn execute_function(func: Rc<CompiledFunction>) -> Result<(), String>  {
	let mut arena = VmArena::create();
	arena.run_function(func)
}
