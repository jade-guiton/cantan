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
	return_idx: usize,
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct VmState<'gc> {
	globals: HashMap<String, Value<'gc>>,
	calls: Vec<Call<'gc>>,
	registers: Vec<Value<'gc>>,
	stack: Vec<Value<'gc>>,
	upvalues: HashMap<usize, GcCell<'gc, Upvalue<'gc>>>,
	idx: usize,
	jumped: bool,
}

type MutVmState<'gc> = GcCell<'gc, VmState<'gc>>;

make_arena!(pub VmArena, MutVmState);

impl<'gc> VmState<'gc> {
	pub fn new() -> Self {
		VmState {
			globals: HashMap::new(),
			calls: vec![],
			registers: vec![],
			stack: vec![],
			upvalues: HashMap::new(),
			idx: 0,
			jumped: false
		}
	}
	
	fn init_globals(&mut self, mc: MutationContext<'gc, '_>) {
		crate::stdlib::create(&mut self.globals, mc);
	}
	
	fn set_reg(&mut self, reg: u16, val: Value<'gc>) {
		let reg = self.calls.last().unwrap().reg_base + reg as usize;
		if reg >= self.registers.len() {
			self.registers.resize(reg + 1, Value::Nil);
		}
		self.registers[reg] = val;
	}
	
	fn get_reg(&self, reg: u16) -> Result<&Value<'gc>, String> {
		let reg = self.calls.last().unwrap().reg_base + reg as usize;
		self.registers.get(reg)
			.ok_or_else(|| String::from("Accessing uninitialized register"))
	}
	
	fn pop(&mut self) -> Result<Value<'gc>, String> {
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
	
	fn has_call(&self) -> bool {
		!self.calls.is_empty()
	}
	
	fn jump(&mut self, rel: i16) {
		if rel <= 0 {
			self.idx -= -rel as usize;
		} else {
			self.idx += rel as usize;
		}
		self.jumped = true;
	}
	
	fn return_from_call(&mut self, implicit: bool) {
		let call = self.calls.pop().unwrap();
		if !call.interactive { // If in interactive mode, keep rest of state for next call
			self.idx = call.return_idx;
			self.registers.truncate(call.reg_base);
			self.jumped = true;
			if implicit {
				self.stack.push(Value::Nil)
			}
		}
	}
	
	// Warning: does not reset state on errors; callers should not reuse state after error
	fn execute_instr(&mut self, mc: MutationContext<'gc, '_>) -> Result<(), String> {
		let func = self.calls.last().unwrap().func;
		let instr = func.chunk.code.get(self.idx)
			.ok_or_else(|| String::from("Jumped to invalid instruction"))?
			.clone();
		
		self.jumped = false;
		
		match instr {
			Instr::Load(reg) => {
				let val = self.get_reg(reg)?.clone();
				self.stack.push(val);
			},
			Instr::LoadUpv(upv_idx) => {
				let upv = func.upvalues.get(upv_idx as usize)
					.ok_or_else(|| String::from("Invalid upvalue index"))?;
				let val = match upv.read().deref() {
					Upvalue::Open(reg) => self.registers.get(*reg as usize)
						.ok_or_else(|| String::from("Upvalue points to invalid register"))?
						.clone(),
					Upvalue::Closed(val) => val.clone(),
				};
				self.stack.push(val);
			},
			Instr::LoadGlobal(name_idx) => {
				let name = self.get_cst(&func.chunk, name_idx)?.get_string()?;
				let val = self.globals.get(name.deref())
					.ok_or_else(|| format!("Accessing undefined global '{}'", name.deref()))?;
				self.stack.push(val.clone());
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
				let reg = self.calls.last().unwrap().reg_base + reg as usize;
				
				// Close upvalue
				if let Some(cell) = self.upvalues.remove(&reg) {
					let val = self.registers[reg].clone();
					*cell.write(mc) = Upvalue::Closed(val);
				}
				
				if reg == self.registers.len() - 1 {
					self.registers.pop();
				} else {
					self.registers[reg] = Value::Nil;
				}
			},
			Instr::Discard => {
				self.pop()?;
			},
			Instr::Log => {
				println!("{}", self.pop()?.repr());
			},
			Instr::Jump(rel) => {
				self.jump(rel);
			},
			Instr::JumpIf(rel) => {
				let val = self.pop()?;
				if val.get_bool()? {
					self.jump(rel);
				}
			},
			Instr::JumpIfNot(rel) => {
				let val = self.pop()?;
				if !val.get_bool()? {
					self.jump(rel);
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
							let reg = self.calls.last().unwrap().reg_base + *reg as usize;
							if let Some(upv) = self.upvalues.get(&reg) {
								upvalues.push(*upv);
							} else {
								let upv = GcCell::allocate(mc, Upvalue::Open(reg));
								self.upvalues.insert(reg, upv);
								upvalues.push(upv);
							}
						},
						CompiledUpvalue::Indirect(upv_idx) => {
							let upv = func.upvalues.get(*upv_idx as usize).copied()
								.ok_or_else(|| String::from("Child function references undefined upvalue"))?;
							upvalues.push(upv);
						},
					}
				}
				let func2 = Gc::allocate(mc, Function { chunk: chunk2, upvalues });
				self.stack.push(Value::Function(func2));
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
						self.call_function(func2, args);
					},
					Value::NativeFunction(func2) => {
						self.stack.push((func2.write(mc).func)(mc, args)?);
					},
					_ => {
						return Err(format!("Cannot call non-function: {}", func2.repr()));
					}
				}
			},
			Instr::Return => {
				self.return_from_call(false);
			},
			Instr::Constant(idx) => {
				self.stack.push(self.get_cst(&func.chunk, idx)?);
			},
			Instr::NewTuple(cnt) => {
				let values = self.pop_n(cnt as usize)?;
				self.stack.push(Value::Tuple(Gc::allocate(mc, values)));
			},
			Instr::NewList(cnt) => {
				let values = self.pop_n(cnt as usize)?;
				self.stack.push(Value::List(GcCell::allocate(mc, values)));
			},
			Instr::NewMap(cnt) => {
				let mut values = self.pop_n(2 * (cnt as usize))?;
				let (mut keys, mut values): (Vec<(usize,Value)>,Vec<(usize,Value)>) =
					values.drain(..).enumerate().partition(|(i,_)| i % 2 == 0);
				let map: HashMap<Value, Value> = keys.drain(..).map(|(_,v)| v).zip(values.drain(..).map(|(_,v)| v)).collect();
				self.stack.push(Value::Map(GcCell::allocate(mc, map)));
			},
			Instr::NewObject(class_idx) => {
				let class = func.chunk.classes.get(class_idx as usize).ok_or_else(|| String::from("Using undefined object class"))?;
				let mut values = self.pop_n(class.len())?;
				let obj: HashMap<String, Value> = class.iter().cloned()
					.zip(values.drain(..)).collect();
				self.stack.push(Value::Object(GcCell::allocate(mc, obj)));
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
						self.stack.push(Value::Bool(c));
					},
					BinaryOp::Plus if a.get_type() == Type::String => {
						let a = a.get_string().unwrap();
						let b = b.get_string()?;
						self.stack.push(Value::String(NiceStr((a.to_string() + &b).into_boxed_str())));
					}
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
						self.stack.push(Value::Int(c));
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
						self.stack.push(c);
					},
					BinaryOp::Eq => {
						self.stack.push(Value::Bool(a.struct_eq(&b)));
					},
					BinaryOp::NotEq => {
						self.stack.push(Value::Bool(!a.struct_eq(&b)));
					},
					BinaryOp::LessEq => {
						self.stack.push(Value::Bool(a.cmp(&b)? <= Ordering::Equal));
					},
					BinaryOp::Less => {
						self.stack.push(Value::Bool(a.cmp(&b)? < Ordering::Equal));
					},
					BinaryOp::Greater => {
						self.stack.push(Value::Bool(a.cmp(&b)? > Ordering::Equal));
					},
					BinaryOp::GreaterEq => {
						self.stack.push(Value::Bool(a.cmp(&b)? >= Ordering::Equal));
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
				self.stack.push(res);
			},
			Instr::Index => {
				let idx = self.pop()?;
				let coll = self.pop()?;
				self.stack.push(coll.index(&idx)?);
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
				self.stack.push(obj.prop(&idx)?);
			},
			Instr::SetProp(cst_idx) => {
				let val = self.pop()?;
				let obj = self.pop()?;
				let idx = self.get_cst(&func.chunk, cst_idx)?.get_string()?;
				obj.set_prop(&idx, val, mc)?;
			},
			Instr::Next(iter_reg) => {
				let iter = self.get_reg(iter_reg)?;
				let next = iter.next(mc)?;
				if let Some(new_iter) = next.0 {
					self.set_reg(iter_reg, new_iter);
				}
				if let Some(value) = next.1 {
					self.stack.push(value);
					self.stack.push(Value::Bool(true));
				} else {
					self.stack.push(Value::Bool(false));
				}
			},
		}
		
		if !self.jumped {
			self.idx += 1;
		}
		
		// Implicit return
		if self.idx == self.calls.last().unwrap().func.chunk.code.len() {
			self.return_from_call(true);
		}
		
		Ok(())
	}
	
	// Like execute_instr, but clears calls and the stack on errors (not registers)
	// Using this, VmState can be reused in interactive mode even on error
	fn execute_instr_safe(&mut self, mc: MutationContext<'gc, '_>) -> Result<(), String> {
		match self.execute_instr(mc) {
			Ok(()) => Ok(()),
			Err(err) => {
				self.calls.clear();
				self.stack.clear();
				Err(err)
			}
		}
	}
	
	fn call_function(&mut self, func: Gc<'gc, Function<'gc>>, mut args: Vec<Value<'gc>>) {
		self.calls.push(Call {
			func,
			interactive: false,
			reg_base: self.registers.len(),
			return_idx: self.idx + 1,
		});
		
		for (i, arg) in args.drain(..).enumerate() {
			self.set_reg(i as u16, arg);
		}
		
		self.idx = 0;
		self.jumped = true;
	}
	
	
	// Runs function at top level in interactive mode from a given index
	// This function can be called multiple times with new versions of the same function
	// Used in REPL
	fn run_interactive(&mut self, mc: MutationContext<'gc, '_>, func: Rc<CompiledFunction>, from: usize) {
		self.calls.push(Call {
			func: Gc::allocate(mc, Function::main(func)),
			interactive: true,
			reg_base: 0,
			return_idx: 0,
		});
		
		self.idx = from;
	}
}

fn new_arena() -> VmArena {
	VmArena::new(gc_arena::ArenaParameters::default(),
		|mc| GcCell::allocate(mc, VmState::new()))
}

pub struct ReplVm {
	arena: VmArena,
}

impl ReplVm {
	pub fn new() -> Self {
		let mut vm = ReplVm { arena: new_arena() };
		vm.arena.mutate(|mc, state| state.write(mc).init_globals(mc));
		vm
	}
	
	pub fn execute_from(&mut self, func: Rc<CompiledFunction>, from: usize) -> Result<(), String>  {
		self.arena.mutate(|mc, state| {
			state.write(mc).run_interactive(mc, func, from);
		});
		while self.arena.root.read().has_call() {
			self.arena.mutate(|mc, state| state.write(mc).execute_instr_safe(mc))?
		}
		Ok(())
	}
}

pub fn execute_function(func: Rc<CompiledFunction>) -> Result<(), String>  {
	let mut arena = new_arena();
	arena.mutate(|mc, state| {
		state.write(mc).init_globals(mc);
		state.write(mc).call_function(Gc::allocate(mc, Function::main(func)), vec![]);
	});
	while arena.root.read().has_call() {
		arena.mutate(|mc, state| state.write(mc).execute_instr(mc))?;
	}
	Ok(())
}
