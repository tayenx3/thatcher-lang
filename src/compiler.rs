use crate::global::*;
use std::fs::File;
use std::io::Write;


pub struct Compiler<'a> {
    pub table: Vec<SymbolTable<'a>>
}

impl<'a> Compiler<'a> {
    pub fn new() -> Self {
        Compiler {
            table: vec![SymbolTable { table: Vec::new() }]
        }
    }
    
    pub fn compile(&mut self, tree: ASTNode<'a>, file_name: &'a str) -> std::io::Result<()> {
        if let ASTNode::Root(ast_stmts) = tree {
            let mut compiled = File::create(format!("{}.erl", file_name))?;
            
            // Write module header
            writeln!(compiled, "-module({}).", file_name)?;
            writeln!(compiled, "-export([main/0]).\n")?;
            
            // Start main function
            writeln!(compiled, "main() ->")?;
            
            for (i, node) in ast_stmts.iter().enumerate() {
                match node {
                    ASTNode::BinOp { op, lhs: _, rhs: _ } => {
                        if ["+", "-", "*", "/"].contains(&&*op.to_string()) {
                            write!(compiled, "io:format(\"~p~n\", [{}])", self.compile_node(node))?;
                        } else {
                            write!(compiled, "{}", self.compile_node(node))?
                        }
                    },
                    _ => write!(compiled, "{}", self.compile_node(node))?
                }
                if i < ast_stmts.len() - 1 {
                    write!(compiled, ",\n")?;
                }
            }
            
            // End main function
            writeln!(compiled, "\n.")?;
            //println!("{:#?}", self.table);
            
            Ok(())
        } else { 
            unreachable!("Expected Root node") 
        }
    }

    fn compile_node(&mut self, node: &ASTNode<'a>) -> String {
        match node {
            ASTNode::IntLit(int) => int.to_string(),
            ASTNode::FloatLit(float) => float.to_string(),
            ASTNode::Bool(boolean) => boolean.to_string(),
            ASTNode::BinOp { op, lhs, rhs } => {
                let op_str = &*op.to_string();
                let mut result: String = String::new();
                match op_str {
                    "+" | "-" | "*" | "/" | "==" | ">" | "<" | ">=" | "<=" | "!=" | "++" | "||" | "&&"=> {
                    // Compile left operand
                    result.push_str("(");
                    result.push_str(&self.compile_node(lhs));
                    
                    // Write operator (convert to Erlang syntax)
                    let erl_op = match op_str {
                        "+" => "+",
                        "-" => "-",
                        "*" => "*",
                        "/" => "/",
                        "==" => "==",
                        "!=" => "/=",
                        ">" => ">",
                        "<" => "<",
                        ">=" => ">=",
                        "<=" => "=>",
                        "++" => "++",
                        "||" => "orelse",
                        "&&" => "andalso",
                        _ => unreachable!()
                    };
                    result.push_str(&format!(" {} ", erl_op));
                    
                    // Compile right operand
                    result.push_str(&self.compile_node(rhs));
                    result.push_str(")");
                },
                "=" => {
                    let target = match **lhs {
                        ASTNode::Identifier(i) => i,
                        _ => panic!("Cannot mutate target")
                    };
                    let find = self.table.last().unwrap().find_symbol(target);
                    let target = if let Ok(symb) = find {
                        if !symb.mutable {
                            panic!("Cannot mutate immutable variable: {}", target)
                        }
                        target
                    } else {
                        panic!("Undefined variable: {}", target)
                    };

                    result = format!("put({}, {})", target, &self.compile_node(rhs))
                },
                "^^" => {
                    let left = &self.compile_node(lhs);
                    let right = &self.compile_node(rhs);
                    result = format!("(({} orelse {}) andalso (not ({} andalso {})))", left, right, left, right)
                },
                _ => unreachable!()
                }

                result
            },
            ASTNode::VarDecl {
                var_name,
                var_mut,
                var_type,
                initializer 
            } => {
                self.table.last_mut().unwrap().table.push(Symbol {
                    name: var_name.to_string(),
                    symbol_type: *var_type,
                    mutable: *var_mut,
                    defined_at: Span { line: 0, column: 0, startpos: 0, endpos: 0 }
                });
                
                let mut result = String::new();
                if !*var_mut {
                    result.push_str(&format!("Thatcher_{} = ", var_name));
                    if let Some(init) = initializer.as_ref() {
                        result.push_str(&self.compile_node(init));
                    } else {
                        result.push_str("undefined");
                    }
                } else {
                    result.push_str(&format!("put({}, ", var_name));
                    if let Some(init) = initializer.as_ref() {
                        result.push_str(&self.compile_node(init));
                    } else {
                        result.push_str("undefined");
                    }
                    result.push_str(")");
                }

                result
            },
            ASTNode::Identifier(i) => {
                let find = self.table.last().unwrap().find_symbol(i);
                if let Ok(symb) = find {
                    if symb.mutable {
                        format!("get({})", i)
                    } else {
                        format!("Thatcher_{}", i)
                    }
                } else {
                    panic!("Undefined variable: {}", i)
                }
            },
            ASTNode::WhileLoop { condition, body } => {
                let condition_code = self.compile_node(&**condition);
                let mut body_code = String::new();
                
                for (i, node) in body.iter().enumerate() {
                    match node {
                        ASTNode::BinOp { op, lhs: _, rhs: _ } => {
                            //println!("{}", op.to_string());
                            if ["+", "-", "*", "/", "==", ">", "<", ">=", "<=", "!="].contains(&&*op.to_string()) {
                                body_code.push_str(&format!("io:format(\"~p~n\", [{}])", self.compile_node(node)));
                            } else {
                                body_code.push_str(&format!("{}", self.compile_node(node)));
                            }
                        },
                        _ => body_code.push_str(&format!("{}", self.compile_node(node)))
                    }
                    if i < body.len() - 1 {
                        body_code.push_str(",\n");
                    }
                }
                
                if !body_code.is_empty() {
                    body_code = format!("begin\n{}\nend", body_code);
                }
                
                format!("(fun WhileLoop() ->\ncase {} of\ntrue -> {}, WhileLoop();\nfalse -> ok\nend\nend)()", 
                        condition_code, body_code)
            }
            ASTNode::IfStatement {
                condition,
                then_body,
                else_body
            } => {
                let mut buffer: String = String::new();
                //println!("{:#?}", condition);
                //println!("{}", self.compile_node(condition));
                buffer.push_str(&format!("case {} of\n", self.compile_node(condition)));
                buffer.push_str("true ->");
                //println!("{:#?}\n\n{:#?}", then_body, else_body);
                if then_body.is_empty() {
                    buffer.push_str(" ok\n")
                } else {
                    buffer.push_str("\nbegin\n");
                    for (i, then_node) in then_body.iter().enumerate() {
                        match then_node {
                            ASTNode::BinOp { op, lhs: _, rhs: _ } => {
                                //println!("{}", op.to_string());
                                if ["+", "-", "*", "/", "==", ">", "<", ">=", "<=", "!="].contains(&&*op.to_string()) {
                                    buffer.push_str(&format!("io:format(\"~p~n\", [{}])", self.compile_node(then_node)));
                                } else {
                                    buffer.push_str(&format!("{}", self.compile_node(then_node)));
                                }
                            },
                            _ => buffer.push_str(&format!("{}", self.compile_node(then_node)))
                        }
                        buffer.push_str(
                            if i < then_body.len() - 1 {
                                ",\n"
                            } else {
                                "\nend;\n"
                            }
                        )
                    }
                }
                buffer.push_str("false ->");
                if else_body.is_empty() {
                    buffer.push_str(" ok\n");
                } else {
                    buffer.push_str("\nbegin\n");
                    for (i, else_node) in else_body.iter().enumerate() {
                        match else_node {
                            ASTNode::BinOp { op, lhs: _, rhs: _ } => {
                                //println!("{}", op.to_string());
                                if ["+", "-", "*", "/", "==", ">", "<", ">=", "<=", "!="].contains(&&*op.to_string()) {
                                    buffer.push_str(&format!("io:format(\"~p~n\", [{}])", self.compile_node(else_node)));
                                } else {
                                    buffer.push_str(&format!("{}", self.compile_node(else_node)));
                                }
                            },
                            _ => buffer.push_str(&format!("{}", self.compile_node(else_node))),
                        }
                        buffer.push_str(
                            if i < else_body.len() - 1 {
                                ",\n"
                            } else {
                                "\nend\n"
                            }
                        )
                    }
                }
                buffer.push_str("end");

                buffer
            }
            other => todo!("{:?}, please contact me if you reach this error", other)
        }
    }
}