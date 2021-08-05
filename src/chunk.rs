use std::fmt::Write;
use std::rc::Rc;

use crate::ast::{UnaryOp, BinaryOp};
use crate::colors::*;
use crate::value::Value;
use crate::gc::Trace;

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
	JumpOr(i16), // Jump if stack top == true (does not pop)
	JumpAnd(i16), // Jump if stack top == false (does not pop)
	
	// Functions
	NewFunction(u16), // Instantiate function by index
	Call(u16), // Call function with N arguments, all on stack
	Return, // Return with stack top
	
	// Values
	Constant(u16), // Constant by index → Stack top
	NewTuple(u16), // Take N values from stack → Stack top
	NewList(u16), // Take N values from stack → Stack top
	NewMap(u16), // Take 2N values from stack → Stack top
	NewStruct(u16), // Struct from class by idx → Stack top
	
	// Operations
	Binary(BinaryOp), // Binary operation on stack
	Unary(UnaryOp), // Unary operation on stack
	Index, // stack[-2] [ stack[-1] ] → stack
	SetIndex, // stack[-3] [ stack[-2] ] ← stack[-1]
	Prop(u16), // stack[-1] [ cst(u16) ] → stack
	SetProp(u16), // stack[-2] [ cst(u16) ] ← stack[-1]
	Next(u16), // next(reg(u16)) → (true, value) / false
}

#[derive(Clone)]
pub enum CompiledUpvalue {
	Direct(u16), // Refer to register in parent function
	Indirect(u16), // Refer to upvalue in parent function
}

#[derive(Clone)]
pub struct CompiledFunction {
	pub name: String,
	pub child_funcs: Vec<Rc<CompiledFunction>>,
	pub arg_cnt: u16,
	pub csts: Vec<Value>,
	pub upvalues: Vec<CompiledUpvalue>,
	pub classes: Vec<Vec<String>>,
	pub code: Vec<Instr>,
	pub code_pos: Vec<usize>,
}

impl CompiledFunction {
	pub fn new(arg_cnt: u16, name: Option<String>) -> Self {
		CompiledFunction {
			name: name.unwrap_or_else(|| String::from("<anon>")),
			child_funcs: vec![],
			arg_cnt,
			csts: vec![],
			upvalues: vec![],
			classes: vec![],
			code: vec![],
			code_pos: vec![],
		}
	}
	
	pub fn list(&self, path: String) {
		let path_desc = if path.len() > 0 { format!(" ({})", path) } else { String::new() };
		println!("{}Function {}{}{} ({} args):{}",
			BRIGHT_GREEN, self.name, path_desc, BRIGHT_WHITE, self.arg_cnt, RESET);
		for (idx, instr) in self.code.iter().enumerate() {
			let repr = {
				let repr = format!("{:?}", instr);
				let mut parts: Vec<String> = repr.strip_suffix(")").unwrap_or(&repr)
					.split('(').map(|s| s.to_string()).collect();
				match instr {
					Instr::LoadGlobal(cst_idx) | Instr::Constant(cst_idx) | Instr::Prop(cst_idx) | Instr::SetProp(cst_idx) => {
						parts[1] = format!("#{}", self.csts.get(*cst_idx as usize).expect("Invalid constant in function").repr());
					},
					Instr::LoadUpv(upv_idx) | Instr::StoreUpv(upv_idx) => {
						let upv = self.upvalues.get(*upv_idx as usize).expect("Invalid upvalue in function");
						parts[1] = match upv {
							CompiledUpvalue::Direct(reg_idx) => format!("r{}", reg_idx),
							CompiledUpvalue::Indirect(upv_idx) => format!("u{}", upv_idx),
						};
					},
					Instr::Jump(rel_jmp) | Instr::JumpIf(rel_jmp) | Instr::JumpIfNot(rel_jmp) | Instr::JumpOr(rel_jmp) | Instr::JumpAnd(rel_jmp) => {
						parts[1] = format!("@{}", (idx as isize) + *rel_jmp as isize);
					},
					Instr::NewStruct(class_idx) => {
						let class = self.classes.get(*class_idx as usize).expect("Invalid class in function");
						let mut class_repr = String::from("(");
						for (i, prop) in class.iter().enumerate() {
							if i != 0 {
								class_repr.push_str(", ");
							}
							class_repr.push_str(prop);
						}
						class_repr.push_str(")");
						parts[1] = class_repr;
					},
					Instr::NewFunction(fn_idx) => {
						parts[1] = format!("{}.{}", path, fn_idx);
					},
					_ => {},
				};
				let mut buf = String::new();
				write!(buf, "{}{}{}", CYAN, parts[0], RESET).unwrap();
				if let Some(args) = parts.get(1) {
					write!(buf, " {}", args).unwrap();
				}
				buf
			};
			let line = self.code_pos[idx];
			let line_desc = if idx == 0 || line != self.code_pos[idx-1] {
				format!("(l{})", line)
			} else { String::new() };	
			println!(" {}{:<4} {:<7}{} {}", BRIGHT_BLACK, idx, line_desc, RESET, repr);
		}
		println!();
		for (idx, child) in self.child_funcs.iter().enumerate() {
			child.list(format!("{}.{}", path, idx));
		}
	}
}

// Note: This works because the Values in CompiledFunction.csts are guaranteed to be primitive values
unsafe impl Trace for CompiledFunction {}
