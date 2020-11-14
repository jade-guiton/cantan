use std::fmt::Write;
use std::rc::Rc;

use crate::ast::{UnaryOp, BinaryOp};
use crate::colors::*;
use crate::value::Value;
use crate::gc::Primitive;

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum Instr {
	// Register and stack management
	Load(u16), // Reg → Stack top
	LoadUpv(u16), // Upvalue → Stack top
	LoadGlobal(u16), // Constant(reg(u16)) → Stack top
	Store(u16), // Stack top → Reg
	StoreUpv(u16), // Stack top → Upvalue
	Drop(u16), // Drop reg
	Discard, // Discard stack top
	Log, // Pop and log stack top (used in REPL)
	
	// Control flow
	Jump(i16), // Relative jump
	JumpIf(i16), // Jump if stack top == true
	JumpIfNot(i16), // Jump if stack top == false
	
	// Functions
	NewFunction(u16), // Instantiate function by index
	Call(u16), // Call function with N arguments, all on stack
	Return, // Return with stack top
	
	// Values
	Constant(u16), // Constant by index → Stack top
	NewTuple(u16), // Take N values from stack → Stack top
	NewList(u16), // Take N values from stack → Stack top
	NewMap(u16), // Take 2N values from stack → Stack top
	NewObject(u16), // Object from class by idx → Stack top
	
	// Operations
	Binary(BinaryOp), // Binary operation on stack
	Unary(UnaryOp), // Unary operation on stack
	Index, // stack[-2] [ stack[-1] ] → stack
	SetIndex, // stack[-3] [ stack[-2] ] ← stack[-1]
	Prop(u16), // stack[-1] [ cst(u16) ] → stack
	SetProp(u16), // stack[-2] [ cst(u16) ] ← stack[-1]
	Next(u16), // next(reg(u16)) → (true, value) / false
}

fn format_cst(func: &CompiledFunction, idx: u16) -> String {
	format!("#{}", func.csts.get(idx as usize).expect("Invalid constant in function").repr())
}

impl Instr {
	fn color_repr(&self, func: &CompiledFunction) -> String {
		let parts: Vec<String> = match self {
			Instr::LoadGlobal(name_idx) =>
				vec![String::from("LoadGlobal"), format_cst(func, *name_idx)],
			Instr::Constant(idx) =>
				vec![String::from("Constant"), format_cst(func, *idx)],
			Instr::Prop(name_idx) =>
				vec![String::from("Prop"), format_cst(func, *name_idx)],
			Instr::SetProp(name_idx) =>
				vec![String::from("SetProp"), format_cst(func, *name_idx)],
			_ => {
				let repr = format!("{:?}", self);
				repr.strip_suffix(")").unwrap_or(&repr).split('(').map(|s| s.to_string()).collect()
			},
		};
		let mut buf = String::new();
		write!(buf, "{}{}{}", CYAN, parts[0], RESET).unwrap();
		if let Some(args) = parts.get(1) {
			write!(buf, " {}", args).unwrap();
		}
		buf
	}
}

#[derive(Clone)]
pub enum CompiledUpvalue {
	Direct(u16), // Refer to register in parent function
	Indirect(u16), // Refer to upvalue in parent function
}

#[derive(Clone)]
pub struct CompiledFunction {
	pub child_funcs: Vec<Rc<CompiledFunction>>,
	pub arg_cnt: u16,
	pub csts: Vec<Value>,
	pub upvalues: Vec<CompiledUpvalue>,
	pub classes: Vec<Vec<String>>,
	pub code: Vec<Instr>,
}

impl CompiledFunction {
	pub fn new(arg_cnt: u16) -> Self {
		CompiledFunction {
			child_funcs: vec![],
			arg_cnt,
			csts: vec![],
			upvalues: vec![],
			classes: vec![],
			code: vec![],
		}
	}
	
	pub fn list(&self) {
		println!("{}Function{}({} args, {} constants, {} object classes):{}",
			BRIGHT_GREEN, BRIGHT_WHITE, self.arg_cnt, self.csts.len(), self.classes.len(), RESET);
		for (idx, instr) in self.code.iter().enumerate() {
			println!("{}{:^4}{} {}", BRIGHT_BLACK, idx, RESET, instr.color_repr(self));
		}
		println!();
		for child in &self.child_funcs {
			child.list();
		}
	}
}

// Note: This works because the Values in CompiledFunction.csts are guaranteed to be primitive values
unsafe impl Primitive for CompiledFunction {}
