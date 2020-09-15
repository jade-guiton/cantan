
use crate::ast::{UnaryOp, BinaryOp, Primitive};

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum Instr {
	// Register and stack management
	Load(u16), // Reg → Stack top
	Store(u16), // Stack top → Reg
	Drop(u16), // Drop reg
	Discard, // Discard stack top
	
	// Control flow
	Jump(i16), // Relative jump
	JumpIf(i16), // Jump if stack top == true
	JumpIfNot(i16), // Jump if stack top == false
	JumpIfNil(i16), // Jump if stack top == nil
	
	// Functions
	NewFn(u16), // Instantiate function by index
	Call, // Call function with arguments, all on stack
	Return, // Return with stack top
	
	// Values
	Constant(u16), // Constant by index → Stack top
	NewTuple(u16), // Take N values from stack → Stack top
	NewList(u16), // Take N values from stack → Stack top
	NewMap(u16), // Take 2N values from stack → Stack top
	
	// Operations
	Binary(BinaryOp), // Binary operation on stack
	BinaryOn(BinaryOp, u16), // reg [op]= stack
	Unary(UnaryOp), // Unary operation on stack
	Next(u16), // next(reg) → stack with reg iterator
}

#[derive(Debug)]
pub struct CompiledFunction {
	pub child_func: Vec<CompiledFunction>,
	pub arg_cnt: u16,
	pub csts: Vec<Primitive>,
	pub code: Vec<Instr>,
}

impl CompiledFunction {
	pub fn new(arg_cnt: u16) -> Self {
		CompiledFunction {
			child_func: vec![],
			arg_cnt,
			csts: vec![],
			code: vec![],
		}
	}
}
