use crate::global::*;
use std::fs::File;
use std::io::Write;

pub struct Compiler{
    pub table: Vec<SymbolTable>
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            table: vec![ SymbolTable { table: Vec::new() } ]
        }
    }
    pub fn compile(&mut self, tree: ASTNode, file_name: String) -> std::io::Result<()> {
        if let ASTNode::Root(ast_stmts) = tree {
            let mut compiled = File::create(format!("{}.erl", file_name))?;
            
            // Write module header
            writeln!(compiled, "-module({}).", file_name)?;
            writeln!(compiled, "-export([main/0]).\n")?;
            
            // Start main function
            writeln!(compiled, "main() ->")?;
            
            for (i, node) in ast_stmts.iter().enumerate() {
                match node {
                    ASTNode::BinOp {
                        op,
                        lhs: _,
                        rhs: _
                    } => {
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
            println!("{:#?}", self.table);
            
            Ok(())
        } else { 
            unreachable!("Expected Root node") 
        }
    }

    fn compile_node(&mut self, node: &ASTNode) -> String {
        match node {
            ASTNode::IntLit(int) => {
                format!("{}", int)
            },
            ASTNode::FloatLit(float) => {
                format!("{}", float)
            },
            ASTNode::BinOp { op, lhs, rhs } => {
                let op_str = &*op.to_string();
                let mut result: String = String::new();
                if ["+", "-", "*", "/"].contains(&op_str) {
                    // Compile left operand
                    result.push_str("(");
                    result.push_str(&self.compile_node(lhs));
                    
                    // Write operator (convert to Erlang syntax)
                    let erl_op = match op_str {
                        "+" => "+",
                        "-" => "-",
                        "*" => "*",
                        "/" => "/",
                        _ => unreachable!()
                    };
                    result.push_str(&format!(" {} ", erl_op));
                    
                    // Compile right operand
                    result.push_str(&self.compile_node(rhs));
                    result.push_str(")");
                } else if op_str == "=" {
                    let mut target = match **lhs {
                        ASTNode::Identifier(i) => i,
                        _ => panic!("Can only mutate mutable variables")
                    };
                    let find = self.table.last().unwrap().find_symbol(target);
                    target = if let Ok(symb) = find {
                        match symb.mutable {
                            true => target,
                            false => panic!("Can only mutate mutable variables")
                        }
                    } else {
                        panic!("Undefined variable: {}", target)
                    };

                    result = String::from(format!("put({}, {})", target, &self.compile_node(rhs)))
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
                    symbol_type: var_type.to_string(),
                    mutable: *var_mut,
                    defined_at: Span { line: 0, column: 0, startpos: 0, endpos: 0 }
                });
                let mut result: String = String::new();
                if !*var_mut {
                    result.push_str(&format!("Thatcher_{} = ", var_name));
                    if (**initializer).is_none() {
                        result.push_str("undefined")
                    } else {
                        result.push_str(&self.compile_node(&(**initializer).clone().unwrap()));
                    }
                } else {
                    result.push_str("put(");
                    result.push_str(var_name);
                    result.push_str(", ");
                    result.push_str(&self.compile_node(&(**initializer).clone().unwrap()));
                    result.push_str(")");
                }

                result
            },
            ASTNode::Identifier(i) => {
                let find = self.table.last().unwrap().find_symbol(i);
                if let Ok(symb) = find {
                    match symb.mutable {
                        true => format!("get({})", i),
                        false => format!("Thatcher_{}", i)
                    }
                } else {
                    panic!("Undefined variable: {}", i)
                }
            },
            other => todo!("{:?}", other)
        }
    }
}