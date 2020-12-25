use ordered_float::NotNan;

#[derive(Debug, Clone)]
pub enum Primitive {
	Nil,
	Bool(bool),
	Int(i32),
	Float(NotNan<f64>),
	String(Box<str>),
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

#[derive(Debug, Clone)]
pub enum Pattern {
	Id(String),
}

#[derive(Debug, Clone)]
pub enum LExpr {
	Id(String),
	Index(Box<Expr>, Box<Expr>),
	Prop(Box<Expr>, String),
}

#[derive(Debug, Clone)]
pub enum Expr {
	Primitive(Primitive),
	Tuple(Vec<Expr>),
	List(Vec<Expr>),
	Map(Vec<(Expr, Expr)>),
	Object(Vec<(String, Expr)>),
	Function(Vec<String>, Block),
	LExpr(LExpr),
	Call(Box<Expr>, Vec<Expr>),
	Unary(UnaryOp, Box<Expr>),
	Binary(BinaryOp, Box<Expr>, Box<Expr>),
	Condition(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Statement {
	Let(Pattern, Expr),
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

pub type Block = Vec<Statement>;
