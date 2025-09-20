use std::fmt;
use colored::Colorize;
//pub static FILE_PATH: &'static str = "main.tc";

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub line: usize,
    pub column: usize,
    pub startpos: usize,
    pub endpos: usize
}

impl Span {
    pub fn unpack_span(&self) -> (usize, usize, usize, usize) {
        (self.line, self.column, self.startpos, self.endpos)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
    Int(i64),
    Float(f64),
    //String(String),
    //Bool(bool),
    Identifier(&'a str),
    Keyword(&'a str),

    Operator(Op),
    LParen,
    RParen,
    Semicolon,

    Unknown(&'a str)
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(num) => write!(f, "integer '{}'", num),
            Self::Float(num) => write!(f, "float '{}'", num),
            Self::Identifier(i) => write!(f, "identifier '{}'", i),
            Self::Keyword(i) => write!(f, "{}", i),
            Self::Operator(o) => write!(f, "operator '{}'", o),
            Self::LParen => write!(f, "'('"),
            Self::RParen => write!(f, "')'"),
            Self::Semicolon => write!(f, "';'"),
            Self::Unknown(s) => write!(f, "UNKNOWN '{}'", s)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Plus,
    Minus,
    Star,
    Slash,
    Equal
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Equal => write!(f, "=")
        }
    }
}

impl Op {
    pub fn get_bp(&self) -> (i32, i32) {
        match self {
            Self::Plus | Self::Minus => (10, 11),
            Self::Star | Self::Slash => (20, 21),
            Self::Equal => (0, 1)
        }
    }
}

#[derive(Debug, Clone)]
pub enum ErrorCode {
    E1000, // Unexpected EOF
    E1001, // Unexpected _
    E1002 // Expected _
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::E1000 => write!(f, "E1000"),
            Self::E1001 => write!(f, "E1001"),
            Self::E1002 => write!(f, "E1002")
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode<'a> {
    Root(Vec<ASTNode<'a>>),

    IntLit(i64),
    FloatLit(f64),
    //StringLit(String),
    //Bool(bool),
    Identifier(&'a str),

    BinOp {
        op: Op,
        lhs: Box<ASTNode<'a>>,
        rhs: Box<ASTNode<'a>>
    },

    VarDecl {
        var_name: &'a str,
        var_mut: bool,
        var_type: &'a str,
        initializer: Box<Option<ASTNode<'a>>>
    }
}


pub trait ErrorMsg {
    fn get_message(&self, name: ErrorCode, msg: &String, place: Span, src: &str) -> String {
        let (line, column, startpos, endpos) = place.unpack_span();
        
        let lines: Vec<&str> = src.lines().collect();
        let total_lines = lines.len();
        let max_digits = total_lines.ilog10() as usize + 1;
        
        let mut output = String::new();
        output.push_str(&format!("{}", format!("error[{}]:\n", name).red().bold()));
        
        // Show 2 lines before and 2 lines after
        let start_idx = line.saturating_sub(1).saturating_sub(2); // Convert to 0-indexed and subtract 2
        let end_idx = (line - 1 + 3).min(total_lines); // Convert to 0-indexed and add 3 (2 after + current)
        
        for i in start_idx..end_idx {
            let line_num = i + 1;
            if let Some(line_content) = lines.get(i) {
                output.push_str(&format!("{:max_digits$} | {}\n", line_num, line_content));
                
                if line_num == line {
                    let spaces = " ".repeat(column.saturating_sub(1));
                    let carets = "^".repeat((endpos+1 - startpos).max(1));
                    output.push_str(&format!("{:max_digits$} | {}{}\n", "", spaces, carets.red()));
                }
            }
        }
        output.push_str(&format!(" = {}", msg.red().bold()));
        output
    }
}

#[derive(Debug, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub symbol_type: String,
    pub mutable: bool,
    pub defined_at: Span
}

#[derive(Debug)]
pub struct SymbolTable {
    pub table: Vec<Symbol>
}

impl SymbolTable {
    pub fn find_symbol(&self, symbol: &str) -> Result<&Symbol, ()> {
        for symb in &self.table {
            if symb.name == symbol {
                return Ok(symb)
            }
        }
        Err(())
    }
}