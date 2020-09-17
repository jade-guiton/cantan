use std::collections::HashMap;
use std::cmp::Ordering;
use std::rc::Rc;

use gc_arena::{Collect, Gc, GcCell, MutationContext};

use crate::ast::{Primitive, UnaryOp, BinaryOp};
use crate::chunk::*;
use crate::value::*;

#[derive(Collect)]
#[collect(no_drop)]
struct Call {
	func: Rc<CompiledFunction>,
	interactive: bool,
	reg_base: usize,
	return_idx: usize,
}

#[derive(Collect)]
#[collect(no_drop)]
pub struct VmState<'gc> {
	calls: Vec<Call>,
	registers: Vec<Value<'gc>>,
	stack: Vec<Value<'gc>>,
	idx: usize,
	jumped: bool,
}

type MutVmState<'gc> = GcCell<'gc, VmState<'gc>>;

make_arena!(pub VmArena, MutVmState);

impl<'gc> VmState<'gc> {
	pub fn new() -> Self {
		VmState {
			calls: vec![],
			registers: vec![],
			stack: vec![],
			idx: 0,
			jumped: false
		}
	}
	
	fn set_reg(&mut self, reg: u16, val: Value<'gc>) {
		let reg = self.calls.last().unwrap().reg_base + reg as usize;
		if reg >= self.registers.len() {
			self.registers.resize(reg + 1, Value::Primitive(Primitive::Nil));
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
		Ok(Value::from(cst))
	}
	
	fn has_call(&self) -> bool {
		self.calls.len() > 0
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
				self.stack.push(Value::Primitive(Primitive::Nil))
			}
		}
	}
	
	// Warning: does not reset state on errors; callers should not reuse state after error
	fn execute_instr(&mut self, ctx: MutationContext<'gc, '_>) -> Result<(), String> {
		let func = self.calls.last().unwrap().func.clone();
		let instr = &func.code.get(self.idx)
			.ok_or_else(|| String::from("Jumped to invalid instruction"))?;
		
		self.jumped = false;
		
		match instr {
			Instr::Load(reg) => {
				let val = self.get_reg(*reg)?.clone();
				self.stack.push(val);
			},
			Instr::Store(reg) => {
				let val = self.pop()?;
				self.set_reg(*reg, val);
			},
			Instr::Drop(reg) => {
				let reg = self.calls.last().unwrap().reg_base + *reg as usize;
				if reg == self.registers.len() - 1 {
					self.registers.pop();
				} else {
					self.registers[reg] = Value::Primitive(Primitive::Nil);
				}
			},
			Instr::Discard => {
				self.pop()?;
			},
			Instr::Log => {
				println!("{}", self.pop()?.repr());
			},
			Instr::Jump(rel) => {
				self.jump(*rel);
			},
			Instr::JumpIf(rel) => {
				let val = self.pop()?;
				if val.get_bool()? {
					self.jump(*rel);
				}
			},
			Instr::JumpIfNot(rel) => {
				let val = self.pop()?;
				if !val.get_bool()? {
					self.jump(*rel);
				}
			},
			Instr::JumpIfNil(rel) => {
				let val = self.pop()?;
				if let Value::Primitive(Primitive::Nil) = val {
					self.jump(*rel);
				}
			},
			Instr::NewFunction(func_idx) => {
				let func2 = func.child_funcs.get(*func_idx as usize)
					.ok_or_else(|| String::from("Accessing undefined function"))?
					.clone();
				self.stack.push(Value::Function(func2));
			},
			Instr::Call(arg_cnt) => {
				let args: Vec<Value> = self.pop_n(*arg_cnt as usize)?;
				let func = self.pop()?;
				if let Value::Function(func) = func {
					if args.len() != func.arg_cnt as usize {
						return Err(format!("Expected {} arguments, got {}", func.arg_cnt, args.len()));
					}
					self.call_function(func, args);
				} else {
					return Err(format!("Cannot call non-function: {}", func.repr()));
				}
			},
			Instr::Return => {
				self.return_from_call(false);
			},
			Instr::Constant(idx) => {
				self.stack.push(self.get_cst(&func, *idx)?);
			},
			Instr::NewTuple(cnt) => {
				let values = self.pop_n(*cnt as usize)?;
				self.stack.push(Value::Tuple(Tuple::new(values)));
			},
			Instr::NewList(cnt) => {
				let values = self.pop_n(*cnt as usize)?;
				self.stack.push(Value::List(Gc::allocate(ctx, values)));
			},
			Instr::NewMap(cnt) => {
				let mut values = self.pop_n(2 * (*cnt as usize))?;
				let (mut keys, mut values): (Vec<(usize,Value)>,Vec<(usize,Value)>) =
					values.drain(..).enumerate().partition(|(i,_)| i % 2 == 0);
				let map: HashMap<Value, Value> = keys.drain(..).map(|(_,v)| v).zip(values.drain(..).map(|(_,v)| v)).collect();
				self.stack.push(Value::Map(Gc::allocate(ctx, map)));
			},
			Instr::NewObject(class_idx) => {
				let class = func.classes.get(*class_idx as usize).ok_or_else(|| String::from("Using undefined object class"))?;
				let mut values = self.pop_n(class.len())?;
				let obj: HashMap<String, Value> = class.iter().cloned()
					.zip(values.drain(..)).collect();
				self.stack.push(Value::Object(Gc::allocate(ctx, obj)));
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
						self.stack.push(Value::Primitive(Primitive::Bool(c)));
					},
					BinaryOp::Plus if a.get_type() == Type::String => {
						let a = a.get_string().unwrap();
						let b = b.get_string()?;
						self.stack.push(Value::Primitive(Primitive::String(a + &b)));
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
						self.stack.push(Value::Primitive(Primitive::Int(c)));
					},
					BinaryOp::Plus | BinaryOp::Minus | BinaryOp::Times | BinaryOp::Divides
					| BinaryOp::IntDivides | BinaryOp::Modulo | BinaryOp::Power => {
						let a = a.get_numeric().map_err(|err|
							if *op == BinaryOp::Plus { expected_types::<()>("Int, Float, or String", &a).unwrap_err() } else { err })?;
						let b = b.get_numeric()?;
						let c = match op {
							BinaryOp::Plus => Primitive::from_float(a + b)?,
							BinaryOp::Minus => Primitive::from_float(a - b)?,
							BinaryOp::Times => Primitive::from_float(a * b)?,
							BinaryOp::Divides => {
								let c = a / b;
								Primitive::from_float(c)?
							},
							BinaryOp::IntDivides => {
								if b == 0.0 {
									return Err(String::from("Division by zero"));
								}
								let c = a.div_euclid(b);
								if c < (i32::MIN as f64) || c > (i32::MAX as f64) {
									return Err(String::from("Result of division does not fit in integer"));
								}
								Primitive::Int(c as i32)
							},
							BinaryOp::Modulo => Primitive::from_float(a.rem_euclid(b))?,
							BinaryOp::Power => Primitive::from_float(a.powf(b))?,
							_ => unreachable!(),
						};
						self.stack.push(Value::Primitive(c));
					},
					BinaryOp::Eq => {
						self.stack.push(Value::Primitive(Primitive::Bool(a.struct_eq(&b))));
					},
					BinaryOp::NotEq => {
						self.stack.push(Value::Primitive(Primitive::Bool(!a.struct_eq(&b))));
					},
					BinaryOp::LessEq => {
						self.stack.push(Value::Primitive(Primitive::Bool(a.cmp(&b)? <= Ordering::Equal)));
					},
					BinaryOp::Less => {
						self.stack.push(Value::Primitive(Primitive::Bool(a.cmp(&b)? < Ordering::Equal)));
					},
					BinaryOp::Greater => {
						self.stack.push(Value::Primitive(Primitive::Bool(a.cmp(&b)? > Ordering::Equal)));
					},
					BinaryOp::GreaterEq => {
						self.stack.push(Value::Primitive(Primitive::Bool(a.cmp(&b)? >= Ordering::Equal)));
					},
				}
			},
			Instr::Unary(op) => {
				let val = self.pop()?;
				let res;
				match op {
					UnaryOp::Minus => {
						res = match val {
							Value::Primitive(Primitive::Int(i)) => Primitive::Int(-i),
							Value::Primitive(Primitive::Float(f)) => Primitive::Float(-f),
							_ => return Err(format!("Cannot get opposite of: {}", val.repr())),
						}
					},
					UnaryOp::Not => {
						if let Value::Primitive(Primitive::Bool(b)) = val {
							res = Primitive::Bool(!b);
						} else {
							return Err(format!("Cannot get logical negation of: {}", val.repr()))
						}
					},
				}
				self.stack.push(Value::Primitive(res));
			},
			Instr::Index => {
				let idx = self.pop()?;
				let coll = self.pop()?;
				self.stack.push(coll.index(&idx)?);
			},
			Instr::Prop(cst_idx) => {
				let obj = self.pop()?;
				let idx = self.get_cst(&func, *cst_idx)?.get_string()?;
				self.stack.push(obj.prop(&idx)?);
			},
			_ => { todo!() },
		}
		
		if !self.jumped {
			self.idx += 1;
		}
		
		// Implicit return
		if self.idx == self.calls.last().unwrap().func.code.len() {
			self.return_from_call(true);
		}
		
		Ok(())
	}
	
	// Like execute_instr, but clears calls and the stack on errors (not registers)
	// Using this, VmState can be reused in interactive mode even on error
	fn execute_instr_safe(&mut self, ctx: MutationContext<'gc, '_>) -> Result<(), String> {
		match self.execute_instr(ctx) {
			Ok(()) => Ok(()),
			Err(err) => {
				self.calls.clear();
				self.stack.clear();
				Err(err)
			}
		}
	}
	
	fn call_function(&mut self, func: Rc<CompiledFunction>, mut args: Vec<Value<'gc>>) {
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
	fn run_interactive(&mut self, func: Rc<CompiledFunction>, from: usize) {
		self.calls.push(Call {
			func,
			interactive: true,
			reg_base: 0,
			return_idx: 0,
		});
		
		self.idx = from;
	}
}

fn new_arena() -> VmArena {
	VmArena::new(gc_arena::ArenaParameters::default(),
		|ctx| GcCell::allocate(ctx, VmState::new()))
}

pub struct ReplVm {
	arena: VmArena,
}

impl ReplVm {
	pub fn new() -> Self {
		ReplVm { arena: new_arena() }
	}
	
	pub fn execute_from(&mut self, func: Rc<CompiledFunction>, from: usize) -> Result<(), String>  {
		self.arena.mutate(|ctx, state| {
			state.write(ctx).run_interactive(func, from);
		});
		while self.arena.root.read().has_call() {
			self.arena.mutate(|ctx, state| state.write(ctx).execute_instr_safe(ctx))?
		}
		Ok(())
	}
}

pub fn execute_function(func: Rc<CompiledFunction>) -> Result<(), String>  {
	let mut arena = new_arena();
	arena.mutate(|ctx, state| {
		state.write(ctx).call_function(func, vec![]);
	});
	while arena.root.read().has_call() {
		arena.mutate(|ctx, state| state.write(ctx).execute_instr(ctx))?;
	}
	Ok(())
}
