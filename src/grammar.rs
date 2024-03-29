use std::fs::read_to_string;

use ordered_float::NotNan;
use unicode_xid::UnicodeXID;

use crate::ast::*;

fn get_char(s: &str) -> char {
	s.chars().next().unwrap()
}

static RESERVED: [&str; 20] = [
	"let",
	"if", "else", "elseif", "while", "do", "loop", "for", "in", "end",
	"break", "continue", "ret",
	"not", "and", "or",
	"nil", "true", "false",
	"fn",
];

fn compute_line_starts(file: &str) -> Vec<usize> {
	let mut line_starts = vec![0];
	for (i, c) in file.as_bytes().iter().enumerate() {
		if *c == b'\n' {
			line_starts.push(i + 1);
		}
	}
	line_starts.push(file.len());
	line_starts
}

pub fn parse(path: &str) -> Result<Block, String> {
	let file = read_to_string(path).map_err(|_| String::from("Unable to open file"))?;
	let line_starts = compute_line_starts(&file);
	cantan_parser::program(&file, &line_starts).map_err(|err| format!("{}", err))
}

pub fn parse_partial_stat(src: &str) -> Option<Result<Statement, String>> {
	let line_starts = compute_line_starts(&src);
	match cantan_parser::lone_statement(src, &line_starts) {
		Ok(stat) => Some(Ok(stat)),
		Err(err) => {
			if err.location.offset == src.len() {
				None
			} else {
				Some(Err(format!("{}", err)))
			}
		}
	}
}

peg::parser! {
	grammar cantan_parser(line_starts: &[usize]) for str {
		pub rule program() -> Block = _ b:block() _ { b }
		rule block() -> Block = s:(positioned_statement() ** statement_sep()) { s }
		rule statement_sep() = ___ ("\n" / ";" / comment()) _
		pub rule lone_statement() -> Statement = _ s:(s:statement() {s} / e:expr() {Statement::ExprStat(e)}) _ { s }
		rule positioned_statement() -> PositionedStatement
			= l:line_no() s:statement() { PositionedStatement(l, s) }
		rule statement() -> Statement
			= "let" wb() _ p:pattern() _ e:val_or_fn() { Statement::Let(p, e) }
			/ "if" _ c:pexpr() _ t:block() _ ei:else_if()* e:else_()? "end" wb() { Statement::If(c,t,ei,e) }
			/ "while" _ c:pexpr() _ b:block() _ "end" wb() { Statement::While(c, b) }
			/ "do" wb() _ b:block() _ "while" c:pexpr() { Statement::DoWhile(b, c) }
			/ "loop" wb() _ b:block() _ "end" wb() { Statement::Loop(b) }
			/ "for" _ "(" _ i:id() _ "in" wb() _ e:expr() _ ")" _ b:block() _ "end" wb() { Statement::For(i,e,b) }
			/ "break" _ c:loop_count()? { Statement::Break(c.unwrap_or(1)) }
			/ "continue" _ c:loop_count()? { Statement::Continue(c.unwrap_or(1)) }
			/ "ret" wb() _ e:expr()? { Statement::Return(e.unwrap_or(Expr::Primitive(Primitive::Nil))) }
			/ l:lexpr() ___ op:assign_op() _ e:expr() {
				if let Some(op) = op {
					Statement::Set(l.clone(), Expr::Binary(op, Box::new(Expr::LExpr(l)), Box::new(e)))
				} else {
					Statement::Set(l, e)
				}
			}
			/ e:expr() {? if let Expr::Call(_,_) = e { Ok(Statement::ExprStat(e)) } else { Err("function call") } }
		rule else_if() -> (Expr, Block) = "elseif" wb() c:pexpr() _ t:block() _ { (c,t) }
		rule else_() -> Block = "else" wb() _ b:block() _ { b }
		rule pattern() -> Pattern
			= i:id() { Pattern::Id(i) }
		rule val_or_fn() -> Expr
			= "=" _ e:expr() { e }
			/ f:fn_def() { f }
		#[cache]
		rule lexpr() -> LExpr
			= e:expr() {? if let Expr::LExpr(l) = e { Ok(l) } else { Err("lexpr") }  }
		rule assign_op() -> Option<BinaryOp>
			= "=" { None }
			/ op:arith_assign_op() { Some(op) }
		rule arith_assign_op() -> BinaryOp
			= quiet!{
				  "+=" { BinaryOp::Plus }
				/ "-=" { BinaryOp::Minus }
				/ "*=" { BinaryOp::Times }
				/ "/=" { BinaryOp::Divides }
			} / expected!("arithmetic/assignement op")
		rule pexpr() -> Expr = "(" _ e:expr() _ ")" { e }
		rule loop_count() -> u32 = "(" _ i:$(['0'..='9']+) _ ")" { i.parse().unwrap() }
		#[cache]
		rule expr() -> Expr = precedence! {
			"if" wb() "(" _ x:expr() _ ")" _ "=" _ y:expr() _ "else" wb() _ z:@ { Expr::Condition(Box::new(x), Box::new(y), Box::new(z)) }
			--
			x:(@) _ op:bool_op() wb() _ y:@ { Expr::Binary(op, Box::new(x), Box::new(y)) }
			--
			"not" wb() _ x:@ { Expr::Unary(UnaryOp::Not, Box::new(x)) }
			--
			x:(@) _ op:comp_op() _ y:@ { Expr::Binary(op, Box::new(x), Box::new(y)) }
			--
			x:(@) _ op:arith_op1() _ y:@ { Expr::Binary(op, Box::new(x), Box::new(y)) }
			--
			x:(@) _ op:arith_op2() _ y:@  { Expr::Binary(op, Box::new(x), Box::new(y)) }
			--
			"-" _ x:@ {
				match x {
					Expr::Primitive(Primitive::Int(i)) => Expr::Primitive(Primitive::Int(-i)),
					Expr::Primitive(Primitive::Float(i)) => Expr::Primitive(Primitive::Float(-i)),
					_ => Expr::Unary(UnaryOp::Minus, Box::new(x)),
				}
			}
			--
			x:@ _ (quiet!{"^"}/expected!("arithmetic op")) _ y:(@) { Expr::Binary(BinaryOp::Power, Box::new(x), Box::new(y)) }
			--
			t:@ ___ "[" _ k:expr() _ "]" { Expr::LExpr(LExpr::Index(Box::new(t), Box::new(k))) }
			o:@ _ "." i:id() { Expr::LExpr(LExpr::Prop(Box::new(o), i)) }
			f:@ ___ "(" _ a:(expr() ** (_ "," _)) _ ("," _)? ")" { Expr::Call(Box::new(f), a) }
			--
			e:pexpr() { e }
			f:("fn" wb() _ f:fn_def() {f}) { f }
			e:("(" _ e:(expr() ** (_ "," _)) _ ("," _)? ")" {e}) { Expr::Tuple(e) }
			e:("[" _ e:(expr() ** (_ "," _)) _ ("," _)? "]" {e}) { Expr::List(e) }
			("[" _ "=" _ "]") { Expr::Map(vec![]) } // Empty map [=]
			e:("[" _ e:(map_item() ** (_ "," _)) _ ("," _)? "]" {e}) { Expr::Map(e) }
			i:("{" _ i:(struct_item() ** (_ "," _)) _ ("," _)? "}" {i}) { Expr::Struct(i) }
			b:boolean() wb() { Expr::Primitive(Primitive::Bool(b)) }
			"nil" wb() { Expr::Primitive(Primitive::Nil) }
			s:string() { Expr::Primitive(Primitive::String(s.into_boxed_str())) }
			f:float() { Expr::Primitive(Primitive::Float(NotNan::new(f).unwrap())) }
			i:int() { Expr::Primitive(Primitive::Int(i)) }
			i:id() { Expr::LExpr(LExpr::Id(i)) }
		}
		
		rule bool_op() -> BinaryOp
			= quiet!{
				  "and" { BinaryOp::And }
				/ "or" { BinaryOp::Or }
			} / expected!("boolean op")
		rule comp_op() -> BinaryOp
			= quiet!{
				  "<=" { BinaryOp::LessEq }
				/ ">=" { BinaryOp::GreaterEq }
				/ "<"  { BinaryOp::Less }
				/ ">"  { BinaryOp::Greater }
				/ "==" { BinaryOp::Eq }
				/ "!=" { BinaryOp::NotEq }
			} / expected!("comparison op")
		rule arith_op1() -> BinaryOp
			= quiet!{
				  "+" { BinaryOp::Plus }
				/ "-" { BinaryOp::Minus }
			} / expected!("arithmetic op")
		rule arith_op2() -> BinaryOp
			= quiet!{
				  "*" { BinaryOp::Times }
				/ "//" { BinaryOp::IntDivides }
				/ "/" { BinaryOp::Divides }
				/ "%" { BinaryOp::Modulo }
			} / expected!("arithmetic op")
		rule boolean() -> bool = quiet!{"true" { true } / "false" { false }} / expected!("boolean")
			
		rule map_item() -> (Expr, Expr)
			= k:expr() _ "=" _ v:expr() { (k, v) }
		rule struct_item() -> (String, Expr)
			= i:id() _ e:val_or_fn() { (i,e) }
		rule fn_def() -> Expr = "(" _ a:(id() ** (_ "," _)) _ ")" _ b:fn_body()
				{ Expr::Function(a, b) }
		rule fn_body() -> Block
			= "=" _ l:line_no() e:expr() { vec![ PositionedStatement(l, Statement::Return(e)) ] }
			/ b:block() _ "end" wb() { b }
		
		rule wb() = &__ / !XIDContinue() // word boundary
		#[cache]
		rule id() -> String
			= quiet!{ i:$(XIDStart() XIDContinue()*)
				{? if RESERVED.contains(&i) { Err("") } else { Ok(String::from(i)) } } }
			/ expected!("identifier")
		rule int() -> i32
			= i:quiet! { i:$(['0'..='9']+) {i} } {? i.parse().map_err(|_| "integer fitting in 32 bits") }
			/ expected!("integer")
			
		rule float() -> f64
			= quiet!{ f:$(['0'..='9']+ "." ['0'..='9']+ (['e'|'E'] ['+'|'-']? ['0'..='9']+)?)
				{? f.parse().map_err(|_| "") }}
			/ expected!("float")
		rule string() -> String
			= quiet!{ "\"" c:string_char()* "\"" { c.into_iter().collect() } } / expected!("string")
		
		rule line_no() -> usize
			= p:position!() { line_starts.partition_point(|s| *s <= p) }
		
		#[cache]
		rule _() = quiet!{([' '|'\t'|'\n']*)**comment()}
		rule __() = quiet!{[' '|'\t'|'\n'] ([' '|'\t'|'\n']*)**comment()}
		rule ___() = quiet!{[' '|'\t']*}
		rule comment() = quiet!{"#" (!['\n'][_])* ['\n']}
		rule XIDStart() = quiet!{ c:$([_]) {?
			if get_char(c).is_xid_start() { Ok(()) } else { Err("") }} }
		rule XIDContinue() = quiet!{ c:$([_]) {?
			if get_char(c).is_xid_continue() { Ok(()) } else { Err("") }} }
		rule string_char() -> char
			= "\\" c:$(['\\'|'"'|'t'|'n']) {? {
				let c = get_char(c);
				match c {
					'\\' | '"' | '\n' => Ok(c),
					't' => Ok('\t'),
					'r' => Ok('\r'),
					'n' => Ok('\n'),
					_ => Err("Invalid escape sequence"),
				}
			}}
			/ !['\\'|'"'] c:$([_]) { get_char(c) }
	}
}
