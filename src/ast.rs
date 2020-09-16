use ordered_float::NotNan;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Primitive {
	Int(i32),
	Float(NotNan<f64>),
	String(String),
	Bool(bool),
	Nil,
}

impl Primitive {
	pub fn from_float(f: f64) -> Result<Self, String> {
		if f.is_finite() {
			Ok(Primitive::Float(NotNan::new(f).unwrap()))
		} else {
			Err(String::from("Float operation had Infinite or NaN result"))
		}
	}
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
	Log(Expr), // Temporary
	ExprStat(Expr),
}

unsafe impl gc_arena::Collect for Primitive {
	fn needs_trace() -> bool { false }
}

#[derive(Debug, Clone)]
pub enum Expr {
	Primitive(Primitive),
	Tuple(Vec<Expr>),
	List(Vec<Expr>),
	Map(Vec<(Expr, Expr)>),
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
