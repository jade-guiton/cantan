use std::fmt::Write;
use std::rc::Rc;

use gc_arena::Collect;

use crate::ast::{UnaryOp, BinaryOp, Primitive};
use crate::colors::*;

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum Instr {
	// Register and stack management
	Load(u16), // Reg → Stack top
	Store(u16), // Stack top → Reg
	Drop(u16), // Drop reg
	Discard, // Discard stack top
	Log, // Pop and log stack top (used in REPL)
	
	// Control flow
	Jump(i16), // Relative jump
	JumpIf(i16), // Jump if stack top == true
	JumpIfNot(i16), // Jump if stack top == false
	JumpIfNil(i16), // Jump if stack top == nil
	
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
}

impl Instr {
	fn color_repr(&self, func: &CompiledFunction) -> String {
		let parts: Vec<String> = match self {
			Instr::Constant(idx) => {
				let cst = func.csts.get(*idx as usize).expect("Invalid constant in function");
				vec![String::from("Constant"), format!("#{}", cst.repr())]
			},
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

#[derive(Debug, Clone)]
pub struct CompiledFunction {
	pub child_funcs: Vec<Rc<CompiledFunction>>,
	pub arg_cnt: u16,
	pub csts: Vec<Primitive>,
	pub classes: Vec<Vec<String>>,
	pub code: Vec<Instr>,
}

impl CompiledFunction {
	pub fn new(arg_cnt: u16) -> Self {
		CompiledFunction {
			child_funcs: vec![],
			arg_cnt,
			csts: vec![],
			classes: vec![],
			code: vec![],
		}
	}
	
	pub fn list(&self) {
		println!("{}Function{}({} args, {} constants, {} object classes):{}",
			BRIGHT_GREEN, BRIGHT_WHITE, self.arg_cnt, self.csts.len(), self.classes.len(), RESET);
		for instr in &self.code {
			println!("  {}", instr.color_repr(self));
		}
		println!();
		for child in &self.child_funcs {
			child.list();
		}
	}
}

unsafe impl Collect for CompiledFunction {
	fn needs_trace() -> bool { false }
}
