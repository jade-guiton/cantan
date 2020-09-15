#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
	Int(i32),
	Float(f64),
	String(String),
	Bool(bool),
	Nil,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
	Minus,
	Not,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
	Plus, Minus, Times, Divides, IntDivides, Modulo, Power,
	Eq, NotEq, LessEq, Less, Greater, GreaterEq,
	And, Or,
}


pub type Block = Vec<Statement>;

#[derive(Debug, Clone)]
pub enum Statement {
	Let(LExpr, Expr),
	Set(LExpr, Expr),
	If(Expr, Block, Vec<(Expr, Block)>, Option<Block>),
	While(Expr, Block),
	DoWhile(Block, Expr),
	Loop(Block),
	For(String, Expr, Block),
	Break(u32),
	Continue(u32),
	Return(Expr),
	ExprStat(Expr),
}

unsafe impl gc_arena::Collect for Primitive {
	fn needs_trace() -> bool { false }
}

#[derive(Debug, Clone)]
pub enum Expr {
	Primitive(Primitive),
	Table(Vec<(Option<Expr>, Box<Expr>)>),
	Object(Vec<(String, Expr)>),
	Fn(Vec<String>, Block),
	LExpr(LExpr),
	SelfRef,
	Call(Box<Expr>, Vec<Expr>),
	Unary(UnaryOp, Box<Expr>),
	Binary(BinaryOp, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum LExpr {
	Id(String),
	Index(Box<Expr>, Box<Expr>),
	Prop(Box<Expr>, String),
}
