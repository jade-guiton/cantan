use std::collections::HashMap;
use std::cmp::Ordering;
use std::ops::Range;

use gc_arena::{Collect, Gc, GcCell, MutationContext};

use crate::ast::{Primitive, BinaryOp};
use crate::chunk::*;
use crate::value::*;

#[derive(Collect)]
#[collect(no_drop)]
pub struct VmState<'gc> {
	stack: Vec<Value<'gc>>,
	registers: Vec<Value<'gc>>,
}

type MutVmState<'gc> = GcCell<'gc, VmState<'gc>>;

make_arena!(pub VmArena, MutVmState);

impl<'gc> VmState<'gc> {
	pub fn new() -> Self {
		VmState { stack: vec![], registers: vec![] }
	}
	
	fn set_reg(&mut self, reg: u16, val: Value<'gc>) {
		let reg = reg as usize;
		if reg >= self.registers.len() {
			self.registers.resize(reg + 1, Value::Primitive(Primitive::Nil));
		}
		self.registers[reg] = val;
	}
	
	fn pop(&mut self) -> Result<Value<'gc>, String> {
		self.stack.pop()
			.ok_or_else(|| String::from("Trying to pop empty stack"))
	}
	
	fn get_cst(&self, func: &CompiledFunction, idx: u16) -> Result<Value<'gc>, String> {
		let cst = func.csts.get(idx as usize)
			.ok_or_else(|| String::from("Accessing undefined constant"))?;
		Ok(Value::from(cst))
	}
	
	fn execute_code(&mut self, ctx: MutationContext<'gc, '_>, func: &CompiledFunction, range: Range<usize>) -> Result<(), String> {
		let mut idx = range.start;
		
		while idx < range.end {
			
			let instr = &func.code[idx];
			
			let mut jumped = false;
			let mut jump = |rel: i16| {
				if rel <= 0 {
					idx -= -rel as usize;
				} else {
					idx += rel as usize;
				}
				jumped = true;
			};
			
			match instr {
				Instr::Load(reg) => {
					let reg = *reg as usize;
					let val = self.registers.get(reg)
						.ok_or_else(|| String::from("Accessing uninitialized register"))?
						.clone();
					self.stack.push(val);
				},
				Instr::Store(reg) => {
					let val = self.pop()?;
					self.set_reg(*reg, val);
				},
				Instr::Drop(reg) => {
					self.registers[*reg as usize] = Value::Primitive(Primitive::Nil);
				},
				Instr::Discard => {
					self.pop()?;
				},
				Instr::Log => {
					println!("{}", self.pop()?.repr());
				},
				Instr::Jump(rel) => {
					jump(*rel);
				},
				Instr::JumpIf(rel) => {
					let val = self.pop()?;
					if val.get_bool()? {
						jump(*rel);
					}
				},
				Instr::JumpIfNot(rel) => {
					let val = self.pop()?;
					if !val.get_bool()? {
						jump(*rel);
					}
				},
				Instr::JumpIfNil(rel) => {
					let val = self.pop()?;
					if let Value::Primitive(Primitive::Nil) = val {
						jump(*rel);
					}
				},
				Instr::Constant(idx) => {
					self.stack.push(self.get_cst(func, *idx)?);
				},
				Instr::NewTuple(cnt) => {
					let start = self.stack.len() - (*cnt as usize);
					let list: Vec<Value> = self.stack.drain(start..).collect();
					self.stack.push(Value::Tuple(Tuple::new(list)));
				},
				Instr::NewList(cnt) => {
					let start = self.stack.len() - (*cnt as usize);
					let list: Vec<Value> = self.stack.drain(start..).collect();
					self.stack.push(Value::List(Gc::allocate(ctx, list)));
				},
				Instr::NewMap(cnt) => {
					let start = self.stack.len() - 2*(*cnt as usize);
					let (mut keys, mut values): (Vec<(usize,Value)>,Vec<(usize,Value)>) =
						self.stack.drain(start..).enumerate().partition(|(i,_)| i % 2 == 0);
					let map: HashMap<Value, Value> = keys.drain(..).map(|(_,v)| v).zip(values.drain(..).map(|(_,v)| v)).collect();
					self.stack.push(Value::Map(Gc::allocate(ctx, map)));
				},
				Instr::NewObject(class_idx) => {
					let class = func.classes.get(*class_idx as usize).ok_or_else(|| String::from("Using undefined object class"))?;
					let start = self.stack.len() - class.len();
					let obj: HashMap<String, Value> = class.iter().cloned().zip(self.stack.drain(start..)).collect();
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
								if op == &BinaryOp::Plus { expected_types::<()>("Int, Float, or String", &a).unwrap_err() } else { err })?;
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
				Instr::Index => {
					let idx = self.pop()?;
					let coll = self.pop()?;
					self.stack.push(coll.index(&idx)?);
				},
				Instr::Prop(cst_idx) => {
					let obj = self.pop()?;
					let idx = self.get_cst(func, *cst_idx)?.get_string()?;
					self.stack.push(obj.prop(&idx)?);
				},
				_ => { todo!() },
			}
			
			if !jumped {
				idx += 1;
			}
		}
		Ok(())
	}
	
	fn execute_function(&mut self, ctx: MutationContext<'gc, '_>, func: &CompiledFunction) -> Result<(), String> {
		self.execute_code(ctx, func, 0..func.code.len())?;
		Ok(())
	}
}

pub struct Vm {
	arena: VmArena,
}

impl Vm {
	pub fn new() -> Self {
		Vm {
			arena: VmArena::new(gc_arena::ArenaParameters::default(),
				|ctx| GcCell::allocate(ctx, VmState::new())),
		}
	}
	
	pub fn execute_code(&mut self, func: &CompiledFunction, code: Range<usize>) -> Result<(), String> {
		self.arena.mutate(|ctx, state| {
			state.write(ctx).execute_code(ctx, func, code)
		})
	}
	
	pub fn execute_function(&mut self, func: &CompiledFunction) -> Result<(), String>  {
		self.arena.mutate(|ctx, state| {
			state.write(ctx).execute_function(ctx, func)
		})
	}
}
