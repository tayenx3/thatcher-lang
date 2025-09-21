use std::fmt;
use colored::Colorize;
use std::collections::HashMap;
//pub static FILE_PATH: &'static str = "main.tc";

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Copy)]
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token<'a> {
    Int(i64),
    Float(f64),
    //String(String),
    Bool(bool),
    Identifier(&'a str),
    Keyword(&'a str),

    Operator(Op),
    LParen,
    RParen,
    Semicolon,
    Colon,
    Comma,
    ExclamationMark,

    Unknown(&'a str)
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(num) => write!(f, "integer '{}'", num),
            Self::Float(num) => write!(f, "float '{}'", num),
            Self::Bool(boolean) => write!(f, "boolean '{}'", boolean),
            Self::Identifier(i) => write!(f, "identifier '{}'", i),
            Self::Keyword(i) => write!(f, "keyword '{}'", i),
            Self::Operator(o) => write!(f, "operator '{}'", o),
            Self::LParen => write!(f, "'('"),
            Self::RParen => write!(f, "')'"),
            Self::Semicolon => write!(f, "';'"),
            Self::Colon => write!(f, ":"),
            Self::Comma => write!(f, ","),
            Self::ExclamationMark => write!(f, "!"),
            Self::Unknown(s) => write!(f, "unrecognised symbol '{}'", s)
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Type<'a> {
    Int,
    Float,
    Str,
    Bool,

    Alias(&'a str)
}

impl<'a> fmt::Display for Type<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::Str => write!(f, "Str"),
            Self::Bool => write!(f, "Bool"),
            Self::Alias(name) => write!(f, "{}", name)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeRegistry<'a> {
    pub built_in: HashMap<&'a str, Type<'a>>,
    pub aliases: HashMap<&'a str, Type<'a>>
}

#[allow(dead_code)]
impl<'a> TypeRegistry<'a> {
    pub fn new() -> Self {
        let mut built_in: HashMap<&'_ str, Type<'_>> = HashMap::new();
        built_in.insert("Int", Type::Int);
        built_in.insert("Float", Type::Float);
        built_in.insert("Str", Type::Str);
        built_in.insert("Bool", Type::Bool);

        TypeRegistry {
            built_in: built_in,
            aliases: HashMap::new()
        }
    }

    pub fn register_alias(&mut self, alias: &'a str, def: Type<'a>) -> Result<(), String> {
        if self.built_in.contains_key(alias) {
            return Err(String::from("Cannot define an alias to a primitive type"))
        }
        self.aliases.insert(alias, def);
        Ok(())
    }
    
    pub fn resolve(&self, name: &'a str) -> Option<Type<'a>> {
        self.built_in.get(name).cloned()
            .or_else(|| self.aliases.get(name).cloned())
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Op {
    Plus,
    Minus,
    Star,
    Slash,
    Equal,

    CmpEq,
    CmpGt,
    CmpLt,
    CmpGe,
    CmpLe,
    CmpNe,

    Concat,

    BitwiseOr,
    BitwiseAnd,
    BitwiseXor
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Equal => write!(f, "="),

            Self::CmpEq => write!(f, "=="),
            Self::CmpGt => write!(f, ">"),
            Self::CmpLt => write!(f, "<"),
            Self::CmpGe => write!(f, ">="),
            Self::CmpLe => write!(f, "<="),
            Self::CmpNe => write!(f, "!="),

            Self::Concat => write!(f, "++"),

            Self::BitwiseOr => write!(f, "||"),
            Self::BitwiseAnd => write!(f, "&&"),
            Self::BitwiseXor => write!(f, "^^")
        }
    }
}

impl Op {
    pub fn get_bp(&self) -> (i32, i32) {
        match self {
            Self::Plus | Self::Minus => (40, 41),
            Self::Star | Self::Slash => (50, 51),
            Self::Equal => (10, 11),

            Self::CmpEq | Self::CmpGt | Self::CmpLt |
            Self::CmpGe | Self::CmpLe | Self::CmpNe => (30, 31),

            Self::Concat => (60, 61),

            Self::BitwiseOr | Self::BitwiseAnd | Self::BitwiseXor => (20, 21)
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ErrorCode {
    E1000, // Unexpected EOF
    E1001, // Unexpected _
    E1002, // Expected _
    E1003, // Undefined variable
    E1004, // Mismatched types
    E1005, // Value error
    E1006, // Mutating an immutable
    E1007, // Expected type
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode<'a> {
    Root(Vec<ASTNode<'a>>),

    IntLit(i64),
    FloatLit(f64),
    //StringLit(String),
    Bool(bool),
    Identifier(&'a str),

    BinOp {
        op: Op,
        lhs: Box<ASTNode<'a>>,
        rhs: Box<ASTNode<'a>>
    },

    VarDecl {
        var_name: &'a str,
        var_mut: bool,
        var_type: Option<Type<'a>>,
        initializer: Box<Option<ASTNode<'a>>>
    },

    IfStatement {
        condition: Box<ASTNode<'a>>,
        then_body: Vec<ASTNode<'a>>,
        else_body: Vec<ASTNode<'a>>,
    },

    WhileLoop {
        condition: Box<ASTNode<'a>>,
        body: Vec<ASTNode<'a>>
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
        let clamp = if line >= 1 { line } else { 1 };
        let end_idx = (clamp - 1 + 3).min(total_lines); // Convert to 0-indexed and add 3 (2 after + current)
        
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
        output.push_str(&format!("{:max_digits$} | {}", ">".repeat(max_digits).red().bold(), msg.red().bold()));
        output
    }
}

pub trait Snippet {
    fn get_message(&self, src: &str, get_full_line: bool) -> String;
}

impl<'a> Snippet for Symbol<'a> {
    fn get_message(&self, src: &str, get_full_line: bool) -> String {
        let (line, column, startpos, endpos) = self.defined_at.unpack_span();
        
        let lines: Vec<&str> = src.lines().collect();
        let total_lines = lines.len();
        let max_digits = total_lines.ilog10() as usize + 1;
        
        let mut output = String::new();
        
        // Show 2 lines before and 2 lines after
        let start_idx = line.saturating_sub(1).saturating_sub(2); // Convert to 0-indexed and subtract 2
        let clamp = if line >= 1 { line } else { 1 };
        let end_idx = (clamp - 1 + 3).min(total_lines); // Convert to 0-indexed and add 3 (2 after + current)
        
        for i in start_idx..end_idx {
            let line_num = i + 1;
            if let Some(line_content) = lines.get(i) {
                output.push_str(&format!("{:max_digits$} | {}\n", line_num, line_content));
                
                if line_num == line {
                    let spaces = " ".repeat(column.saturating_sub(1));
                    let carets = match get_full_line {
                        false => "^".repeat((endpos - startpos).max(1)),
                        true => {
                            "^".repeat(lines[line-1].len().max(1))
                        }
                    };
                    output.push_str(&format!("{:max_digits$} | {}{}\n", "", spaces, carets.cyan()));
                }
            }
        }
        output
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Symbol<'a> {
    pub name: String,
    pub symbol_type: Option<Type<'a>>,
    pub mutable: bool,
    pub defined_at: Span
}

#[derive(Debug, Clone)]
pub struct SymbolTable<'a> {
    pub table: Vec<Symbol<'a>>
}

impl<'a> SymbolTable<'a> {
    pub fn find_symbol(&self, symbol: &str) -> Result<&Symbol, ()> {
        for symb in &self.table {
            if symb.name == symbol {
                return Ok(symb)
            }
        }
        Err(())
    }
}

pub fn suggest_similar_name(bad_name: &str, available_names: &[String]) -> Option<String> {
    available_names
        .iter()
        .map(|name| (name, strsim::jaro_winkler(bad_name, name)))
        .filter(|(_, score)| *score > 0.7) // Only suggest reasonably similar names
        .max_by(|a, b| a.1.partial_cmp(&b.1).unwrap())
        .map(|(name, _)| name.clone())
}