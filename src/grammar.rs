use std::fs::read_to_string;
use unicode_xid::UnicodeXID;

use crate::ast::*;

fn get_char(s: &str) -> char {
	s.chars().next().unwrap()
}

static RESERVED: [&str; 20] = [
	"let",
	"if", "else", "while", "do", "loop", "for", "end",
	"break", "continue", "return",
	"self",
	"not", "and", "or",
	"nil", "true", "false",
	"fn",
	"log",
];

pub fn parse(path: &str) -> Result<Block, String> {
	let file = read_to_string(path).map_err(|_| String::from("Unable to open file"))?;
	cantan_parser::program(&file).map_err(|err| format!("{}", err))
}

pub fn parse_partial_stat(src: &str) -> Option<Result<Statement, String>> {
	match cantan_parser::lone_statement(src) {
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
	grammar cantan_parser() for str {
		pub rule program() -> Block = _ b:block() _ { b }
		rule block() -> Block = s:(statement() ** _) { s }
		pub rule lone_statement() -> Statement = _ s:statement() _ { s }
		rule statement() -> Statement
			= "let" wb() _ l:lexpr() _ e:(
					  "=" _ e:expr() { e }
					/ f:fn_def() { f }
				) { Statement::Let(l, e) }
			/ "if" _ c:pexpr() _ t:block() _ ei:else_if()* e:else_()? "end" wb() { Statement::If(c,t,ei,e) }
			/ "while" _ c:pexpr() _ b:block() _ "end" wb() { Statement::While(c, b) }
			/ "do" wb() _ b:block() _ "while" c:pexpr() { Statement::DoWhile(b, c) }
			/ "loop" wb() _ b:block() _ "end" wb() { Statement::Loop(b) }
			/ "for" "(" _ i:id() _ ":" _ e:expr() _ ")" _ b:block() "end" wb() { Statement::For(i,e,b) }
			/ "break" c:loop_count()? { Statement::Break(c.unwrap_or(1)) }
			/ "continue" c:loop_count()? { Statement::Continue(c.unwrap_or(1)) }
			/ "return" wb() _ e:pexpr()? { Statement::Return(e.unwrap_or(Expr::Primitive(Primitive::Nil))) }
			/ "log" wb() _ e:pexpr() { Statement::Log(e) } // Temporary
			/ l:lexpr() _ "=" _ e:expr() { Statement::Set(l, e) }
			/ e:expr() { Statement::ExprStat(e) }
		rule else_if() -> (Expr, Block) = "else" __ "if" wb() c:pexpr() _ t:block() _ { (c,t) }
		rule else_() -> Block = "else" wb() _ b:block() _ { b }
		#[cache]
		rule lexpr() -> LExpr
			= e:expr() {? if let Expr::LExpr(l) = e { Ok(l) } else { Err("lexpr") }  }
		rule pexpr() -> Expr = "(" e:expr() ")" { e }
		rule loop_count() -> u32 = "(" _ i:$(['0'..='9']+) _ ")" { i.parse().unwrap() }
		#[cache]
		rule expr() -> Expr = precedence! {
			x:(@) _ "and" wb() _ y:@ { Expr::Binary(BinaryOp::And, Box::new(x), Box::new(y)) }
			x:(@) _ "or" wb() _ y:@  { Expr::Binary(BinaryOp::Or,  Box::new(x), Box::new(y)) }
			--
			"not" wb() x:@ { Expr::Unary(UnaryOp::Not, Box::new(x)) }
			--
			x:(@) _ "<=" _ y:@ { Expr::Binary(BinaryOp::LessEq,    Box::new(x), Box::new(y)) }
			x:(@) _ ">=" _ y:@ { Expr::Binary(BinaryOp::GreaterEq, Box::new(x), Box::new(y)) }
			x:(@) _ "<" _ y:@ { Expr::Binary(BinaryOp::Less,       Box::new(x), Box::new(y)) }
			x:(@) _ ">" _ y:@ { Expr::Binary(BinaryOp::Greater,    Box::new(x), Box::new(y)) }
			x:(@) _ "==" _ y:@ { Expr::Binary(BinaryOp::Eq,        Box::new(x), Box::new(y)) }
			x:(@) _ "!=" _ y:@ { Expr::Binary(BinaryOp::NotEq,     Box::new(x), Box::new(y)) }
			--
			x:(@) _ "+" _ y:@ { Expr::Binary(BinaryOp::Plus,  Box::new(x), Box::new(y)) }
			x:(@) _ "-" _ y:@ { Expr::Binary(BinaryOp::Minus, Box::new(x), Box::new(y)) }
			--
			"-" _ x:@ {
				if let Expr::Primitive(Primitive::Int(i)) = x {
					Expr::Primitive(Primitive::Int(-i))
				} else {
					Expr::Unary(UnaryOp::Minus, Box::new(x))
				}
			}
			--
			x:(@) _ "*" _ y:@  { Expr::Binary(BinaryOp::Times,      Box::new(x), Box::new(y)) }
			x:(@) _ "//" _ y:@ { Expr::Binary(BinaryOp::IntDivides, Box::new(x), Box::new(y)) }
			x:(@) _ "/" _ y:@  { Expr::Binary(BinaryOp::Divides,    Box::new(x), Box::new(y)) }
			x:(@) _ "%" _ y:@  { Expr::Binary(BinaryOp::Modulo,     Box::new(x), Box::new(y)) }
			--
			x:@ _ "^" _ y:(@) { Expr::Binary(BinaryOp::Power,   Box::new(x), Box::new(y)) }
			--
			t:@ ___ "[" _ k:expr() _ "]" { Expr::LExpr(LExpr::Index(Box::new(t), Box::new(k))) }
			o:@ _ "." i:id() { Expr::LExpr(LExpr::Prop(Box::new(o), i)) }
			f:@ ___ "(" _ a:(expr() ** (_ "," _)) _ ("," _)? ")" { Expr::Call(Box::new(f), a) }
			--
			"fn" wb() _ f:fn_def() { f }
			"[" _ e:(expr() ** (_ "," _)) _ ("," _)? "]" { Expr::Tuple(e) }
			"[|" _ e:(expr() ** (_ "," _)) _ ("," _)? "]" { Expr::List(e) }
			"[" _ "=" _ "]" { Expr::Map(vec![]) } // Empty map [:]
			"[" _ e:(map_item() ** (_ "," _)) _ "]" { Expr::Map(e) }
			"{" _ i:(object_item() ** (_ "," _)) _ "}" { Expr::Object(i) }
			e:pexpr() { e }
			"true" wb() { Expr::Primitive(Primitive::Bool(true)) }
			"false" wb() { Expr::Primitive(Primitive::Bool(false)) }
			"self" wb() { Expr::SelfRef }
			"nil" wb() { Expr::Primitive(Primitive::Nil) }
			s:string() { Expr::Primitive(Primitive::String(s)) }
			f:float() { Expr::Primitive(Primitive::from_float(f).unwrap()) }
			i:int() { Expr::Primitive(Primitive::Int(i)) }
			i:id() { Expr::LExpr(LExpr::Id(i)) }
		}
		rule map_item() -> (Expr, Expr)
			= k:expr() _ "=" _ v:expr() { (k, v) }
		rule object_item() -> (String, Expr) = i:id() _ "=" _ e:expr() { (i,e) }
		rule fn_def() -> Expr = "(" _ a:(id() ** (_ "," _)) _ ")" _ b:fn_body()
				{ Expr::Function(a, b) }
		rule fn_body() -> Block
			= "=" _ e:expr() { vec![ Statement::Return(e) ] }
			/ b:block() _ "end" wb() { b }
		
		rule wb() = __ / !XIDContinue() // word boundary
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
			= "\"" c:string_char()* "\"" { c.into_iter().collect() }
		
		#[cache]
		rule _() = quiet!{[' '|'\t'|'\n']*}
		rule __() = quiet!{[' '|'\t'|'\n']+}
		rule ___() = quiet!{[' '|'\t']*}
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
