use std::collections::HashMap;
use std::cmp::Ordering;
use std::convert::TryFrom;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use crate::ast::{UnaryOp, BinaryOp};
use crate::chunk::*;
use crate::value::*;
use crate::gc::{Trace, GcRef, GcCell, GcHeap};

#[derive(Trace)]
struct FunctionCall {
	func: GcRef<Function>,
	interactive: bool,
	reg_base: usize,
	return_idx: Option<usize>,
}

#[derive(Trace)]
struct NativeCallParams {
	func: GcRef<NativeFunctionWrapper>,
	args: Vec<Value>,
}

#[derive(Trace)]
enum NativeCall {
	NativeFunction(NativeCallParams),
	Iterator(GcCell<dyn NativeIterator>),
}

#[derive(Trace)]
enum Call {
	Function(FunctionCall),
	Native,
}

impl Call {
	fn unwrap_function_call(&mut self) -> &mut FunctionCall {
		match self {
			Call::Function(call) => call,
			_ => panic!("Expected non-native function call"),
		}
	}
	fn unwrap_native_call(&mut self) {
		match self {
			Call::Native => (),
			_ => panic!("Expected native function call"),
		}
	}
}

#[derive(Trace)]
pub struct VmState {
	globals: HashMap<String, Value>,
	calls: Vec<Call>,
	registers: Vec<Value>,
	stack: Vec<Value>,
	upvalues: HashMap<usize, GcCell<Upvalue>>,
	reg_base: usize,
	idx: usize,
}

impl VmState {
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
	
	fn init_globals(&mut self, gc: &mut GcHeap) {
		crate::stdlib::create(&mut self.globals, gc);
	}
	
	fn set_reg(&mut self, reg: u16, val: Value) {
		let reg = self.reg_base + reg as usize;
		if reg >= self.registers.len() {
			self.registers.resize(reg + 1, Value::Nil);
		}
		self.registers[reg] = val;
	}
	
	fn get_raw_reg(&self, reg: usize) -> Result<&Value, String> {
		self.registers.get(reg)
			.ok_or_else(|| String::from("Accessing uninitialized register"))
	}
	
	fn get_reg(&self, reg: u16) -> Result<&Value, String> {
		let reg = self.reg_base + reg as usize;
		self.get_raw_reg(reg)
	}
	
	fn close_reg_raw(&mut self, reg: usize) {
		if let Some(cell) = self.upvalues.remove(&reg) {
			let val = self.registers[reg].clone();
			*cell.borrow_mut() = Upvalue::Closed(val);
		}
	}
	
	fn drop_reg(&mut self, reg: u16) {
		let reg = self.reg_base + reg as usize;
		self.close_reg_raw(reg);
		
		if reg == self.registers.len() - 1 {
			self.registers.pop();
		} else {
			self.registers[reg] = Value::Nil;
		}
	}
	
	fn get_upv(&self, upv: &Upvalue) -> Result<Value, String> {
		match upv {
			Upvalue::Open(reg) => Ok(self.get_raw_reg(*reg as usize)?.clone()),
			Upvalue::Closed(val) => Ok(val.clone()),
		}
	}
	
	fn make_upv(&mut self, gc: &mut GcHeap, reg: u16) -> GcCell<Upvalue> {
		let reg = self.reg_base + reg as usize;
		if let Some(upv) = self.upvalues.get(&reg) {
			upv.clone()
		} else {
			let upv = gc.add_cell(Upvalue::Open(reg));
			self.upvalues.insert(reg, upv.clone());
			upv
		}
	}
	
	pub fn get_global(&self, name: &str) -> Result<Value, String> {
		self.globals.get(name).cloned()
			.ok_or_else(|| format!("Accessing undefined global '{}'", name))
	}
	
	pub fn push(&mut self, val: Value) {
		self.stack.push(val);
	}
	
	pub fn pop(&mut self) -> Result<Value, String> {
		self.stack.pop()
			.ok_or_else(|| String::from("Popping from empty stack"))
	}
	
	fn pop_n(&mut self, n: usize) -> Result<Vec<Value>, String> {
		let start = self.stack.len().checked_sub(n)
			.ok_or_else(|| String::from("Popping too many values from stack"))?;
		Ok(self.stack.drain(start..).collect())
	}
	
	fn get_cst(&self, func: &CompiledFunction, idx: u16) -> Result<Value, String> {
		let cst = func.csts.get(idx as usize)
			.ok_or_else(|| String::from("Accessing undefined constant"))?;
		Ok(cst.clone())
	}
	
	fn jump(&mut self, rel: i16) {
		if rel <= 0 {
			self.idx -= -rel as usize;
		} else {
			self.idx += rel as usize;
		}
	}

	fn call_function(&mut self, func: GcRef<Function>, mut args: Vec<Value>) -> Result<(), String> {
		let chunk = &func.chunk;
		if args.len() != chunk.arg_cnt as usize {
			return Err(format!("Expected {} arguments, got {}", chunk.arg_cnt, args.len()));
		}

		if let Some(Call::Function(call)) = self.calls.last_mut() {
			call.return_idx = Some(self.idx + 1);
		}
		let reg_base = self.registers.len();
		self.reg_base = reg_base;
		self.calls.push(Call::Function(FunctionCall {
			func,
			interactive: false,
			reg_base,
			return_idx: None,
		}));
		
		for (i, arg) in args.drain(..).enumerate() {
			self.set_reg(i as u16, arg);
		}
		
		self.idx = 0;

		Ok(())
	}

	fn call_native(&mut self) {
		if let Some(Call::Function(call)) = self.calls.last_mut() {
			call.return_idx = Some(self.idx + 1);
		}
		self.calls.push(Call::Native);
	}
	
	// Runs function at top level in "interactive mode"
	// This means that the function's registers won't be dropped after execution,
	// and some more code within the same scope can be run afterwards.
	// Used in REPL.
	fn call_interactive(&mut self, gc: &mut GcHeap, func: Rc<CompiledFunction>) {
		assert!(self.calls.is_empty());
		
		self.calls.push(Call::Function(FunctionCall {
			func: gc.add(Function::main(func)),
			interactive: true,
			reg_base: 0,
			return_idx: None,
		}));
		
		self.idx = 0;
	}

	fn unwrap_function_call(&mut self) -> &mut FunctionCall {
		self.calls.last_mut().unwrap().unwrap_function_call()
	}

	fn reenter_last_call(&mut self) {
		if let Some(Call::Function(call)) = self.calls.last_mut() {
			if let Some(idx) = call.return_idx.take() {
				self.idx = idx;
			}
			self.reg_base = call.reg_base;
		}
	}
	
	fn return_from_function(&mut self, implicit: bool) {
		let mut call = self.calls.pop().unwrap();
		let call = call.unwrap_function_call();
		if !call.interactive { // If in interactive mode, keep rest of state for next call
			if implicit {
				self.push(Value::Nil)
			}
			for reg in call.reg_base .. self.registers.len() {
				self.close_reg_raw(reg);
			}
			self.registers.truncate(call.reg_base);
		}
		
		self.reenter_last_call();
	}

	fn return_from_native(&mut self, values: Vec<Value>) {
		self.calls.pop().unwrap().unwrap_native_call();
		
		for x in values {
			self.push(x);
		}
		
		self.reenter_last_call();
	}

	pub fn stack_size(&self) -> usize {
		self.calls.len()
	}
	pub fn check_stack_size(&self, base_call: usize) -> bool {
		self.stack_size() == base_call
	}

	pub fn check_end(&mut self) {
		// Implicit return
		while {
				if let Some(Call::Function(call)) = self.calls.last() {
					self.idx == call.func.chunk.code.len()
				} else { false } } {
			self.return_from_function(true);
		}
	}
	
	// Warning: does not reset state on errors; callers should not reuse state after error
	fn run_instr_unsafe(&mut self, gc: &mut GcHeap) -> Result<Option<NativeCall>, String> {
		let func = self.unwrap_function_call().func.clone();
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
				self.push(self.get_upv(&upv.borrow())?);
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
				match upv.borrow_mut().deref_mut() {
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
				self.drop_reg(reg);
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
							upvalues.push(self.make_upv(gc, *reg));
						},
						CompiledUpvalue::Indirect(upv_idx) => {
							let upv = func.upvalues.get(*upv_idx as usize).cloned()
								.ok_or_else(|| String::from("Child function references undefined upvalue"))?;
							upvalues.push(upv);
						},
					}
				}
				let func2 = gc.add(Function { chunk: chunk2, upvalues });
				self.push(Value::Function(func2));
			},
			Instr::Call(arg_cnt) => {
				let args: Vec<Value> = self.pop_n(arg_cnt as usize)?;
				let func2 = self.pop()?;
				match func2 {
					Value::Function(func2) => {
						self.call_function(func2, args)?;
						jumped = true;
					},
					Value::NativeFunction(func2) => {
						self.call_native();
						return Ok(Some(NativeCall::NativeFunction(NativeCallParams {
							func: func2, args,
						})));
					},
					_ => {
						return Err(format!("Cannot call non-function: {}", func2.repr()));
					}
				}
			},
			Instr::Return => {
				self.return_from_function(false);
				jumped = true;
			},
			Instr::Constant(idx) => {
				self.push(self.get_cst(&func.chunk, idx)?);
			},
			Instr::NewTuple(cnt) => {
				let values = self.pop_n(cnt as usize)?;
				self.push(Value::Tuple(gc.add(values)));
			},
			Instr::NewList(cnt) => {
				let values = self.pop_n(cnt as usize)?;
				self.push(Value::List(gc.add_cell(values)));
			},
			Instr::NewMap(cnt) => {
				let mut values = self.pop_n(2 * (cnt as usize))?;
				let (mut keys, mut values): (Vec<(usize,Value)>,Vec<(usize,Value)>) =
					values.drain(..).enumerate().partition(|(i,_)| i % 2 == 0);
				let map: HashMap<Value, Value> = keys.drain(..).map(|(_,v)| v).zip(values.drain(..).map(|(_,v)| v)).collect();
				self.push(Value::Map(gc.add_cell(map)));
			},
			Instr::NewObject(class_idx) => {
				let class = func.chunk.classes.get(class_idx as usize).ok_or_else(|| String::from("Using undefined object class"))?;
				let mut values = self.pop_n(class.len())?;
				let obj: HashMap<String, Value> = class.iter().cloned()
					.zip(values.drain(..)).collect();
				self.push(Value::Object(gc.add_cell(obj)));
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
						self.push(Value::String((a + &b).into_boxed_str()));
					},
					BinaryOp::Plus if a.get_type() == Type::List => {
						let a = a.get_list().unwrap();
						let mut a = a.borrow().deref().clone();
						let b = b.get_list()?;
						a.extend_from_slice(b.borrow().deref());
						self.push(Value::List(gc.add_cell(a)));
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
							Value::Float(f) => Value::Float(-f),
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
				coll.set_index(&idx, val)?;
			},
			Instr::Prop(cst_idx) => {
				let obj = self.pop()?;
				let idx = self.get_cst(&func.chunk, cst_idx)?.get_string()?;
				self.push(obj.prop(&idx, gc)?);
			},
			Instr::SetProp(cst_idx) => {
				let val = self.pop()?;
				let obj = self.pop()?;
				let idx = self.get_cst(&func.chunk, cst_idx)?.get_string()?;
				obj.set_prop(&idx, val)?;
			},
			Instr::Next(iter_reg) => {
				let mut iter = self.get_reg(iter_reg)?;
				if let Some(new_iter) = iter.make_iter(gc)? {
					self.set_reg(iter_reg, new_iter);
					iter = self.get_reg(iter_reg)?;
				}
				let iter = iter.get_iter()?;
				self.call_native();
				return Ok(Some(NativeCall::Iterator(iter)));
			},
		}
		
		if !jumped {
			self.idx += 1;
		}
		
		self.check_end();
		
		Ok(None)
	}
	
	// Like run_instr_unsafe, but clears calls and the stack on errors
	// Does not clear registers
	// Using this, VmState can be reused in interactive mode even after error
	fn run_instr(&mut self, gc: &mut GcHeap) -> Result<Option<NativeCall>, String> {
		match self.run_instr_unsafe(gc) {
			Ok(x) => Ok(x),
			Err(err) => {
				self.calls.clear();
				self.stack.clear();
				Err(err)
			}
		}
	}
}


pub struct VmArena {
	vm: GcCell<VmState>,
	pub gc: GcHeap,
}

impl VmArena {
	fn create() -> Self {
		let mut gc = GcHeap::new();
		let vm = gc.add_cell(VmState::new());
		vm.borrow_mut().init_globals(&mut gc);
		VmArena { gc, vm }
	}
	
	fn run(&mut self, call_base: usize) -> Result<(), String> {
		self.vm.borrow_mut().check_end(); // In case function is empty
		if self.vm.borrow().check_stack_size(call_base) { return Ok(()); }
		
		loop {
			let call = self.vm.borrow_mut().run_instr(&mut self.gc)?;
			self.gc.step();
			
			if self.vm.borrow().check_stack_size(call_base) { break }
			
			if let Some(call) = call {
				let mut res = vec![];
				match call {
					NativeCall::NativeFunction(params) => {
						res.push(params.func.call(self, params.args)?);
					},
					NativeCall::Iterator(iter) => {
						if let Some(value) = iter.borrow_mut().next(self)? {
							res.push(value);
							res.push(Value::Bool(true));
						} else {
							res.push(Value::Bool(false));
						}
					},
				}
				self.vm.borrow_mut().return_from_native(res);
				self.gc.step();
			}
		}
		Ok(())
	}
	
	fn run_function(&mut self, func: Rc<CompiledFunction>) -> Result<(), String>  {
		let call_base = self.vm.borrow().stack_size();
		self.vm.borrow_mut().call_function(self.gc.add(Function::main(func)), vec![])?;
		self.run(call_base)
	}

	pub fn call_function(&mut self, func: GcRef<Function>, args: Vec<Value>) -> Result<Value, String> {
		let call_base = self.vm.borrow().stack_size();
		self.vm.borrow_mut().call_function(func, args)?;
		self.run(call_base)?;
		self.vm.borrow_mut().pop()
	}
	
	fn run_interactive(&mut self, func: Rc<CompiledFunction>) -> Result<(), String>  {
		let call_base = self.vm.borrow().calls.len();
		self.vm.borrow_mut().call_interactive(&mut self.gc, func);
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
