use crate::global::*;
use std::fmt;
use colored::Colorize;

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct ParseError<'a> {
    pub name: ErrorCode,
    pub msg: String,
    pub place: Span,
    pub src: &'a str,
    pub help: String
}

impl fmt::Display for ParseError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.help.is_empty() {
            write!(f, "{}", self.get_message(self.name.clone(), &self.msg, self.place.clone(), self.src))
        } else {
            write!(f, "{}\n\n{} =\n{}", self.get_message(self.name.clone(), &self.msg, self.place.clone(), self.src), "help".cyan().bold(), self.help)
        }
    }
}

impl ErrorMsg for ParseError<'_> {}

pub struct ThatchParser<'a> {
    pos: usize,
    toks: Vec<(Token<'a>, Span)>,
    source: &'a str,
    aliases: TypeRegistry<'a>,
    env: Vec<SymbolTable<'a>>
}

impl<'a> ThatchParser<'a> {
    pub fn new() -> Self {
        ThatchParser {
            pos: 0,
            toks: Vec::new(),
            source: "",
            aliases: TypeRegistry::new(),
            env: vec![SymbolTable { table: Vec::new() }]
        }
    }

    pub fn load(&mut self, toks: Vec<(Token<'a>, Span)>, src: &'a str) {
        self.toks = toks;
        self.source = src;
    }

    fn view(&self, offset: i32) -> Option<&(Token<'a>, Span)> {
        let view_pos = (self.pos as i32 + offset) as usize;
        self.toks.get(view_pos)
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn consume(&mut self, expected: Token<'a>) -> Result<(), ParseError<'a>> {
        match self.view(0) {
            Some((current_token, _place)) if *current_token == expected => {
                self.advance();
                Ok(())
            }
            Some((current_token, place)) => {
                Err(ParseError {
                    name: ErrorCode::E1002,
                    msg: format!("Expected {}, found {}", expected, current_token),
                    place: place.clone(),
                    src: self.source,
                    help: "".to_string()
                })
            }
            None => Err(self.handle_err_eof(Some(expected))),
        }
    }

    fn parse_primary(&mut self) -> Result<ASTNode<'a>, ParseError<'a>> {
        let (current_token, place) = match self.view(0) {
            Some((token, span)) => (token, span.clone()), // Clone the span here
            None => return Err(self.handle_err_eof(None)),
        };

        let node = match *current_token { // Dereference the token
            Token::Int(number) => {
                self.advance();
                ASTNode::IntLit(number)
            },
            Token::Float(number) => {
                self.advance();
                ASTNode::FloatLit(number)
            },
            Token::Bool(boolean) => {
                self.advance();
                ASTNode::Bool(boolean)
            }
            Token::Identifier(ident) => {
                self.advance();
                ASTNode::Identifier(ident)
            },
            Token::LParen => {
                self.advance();
                let expr = self.parse_expression(0)?;
                self.consume(Token::RParen)?;
                expr
            },
            _ => return Err(ParseError {
                name: ErrorCode::E1001,
                msg: format!("Unexpected {}", current_token),
                place,
                src: self.source,
                help: "".to_string()
            }),
        };

        Ok(node)
    }

    fn parse_expression(&mut self, min_bp: i32) -> Result<ASTNode<'a>, ParseError<'a>> {
        let mut lhs = self.parse_primary()?;
        
        while let Some((current_token, _)) = self.view(0) {
            let (lbp, rbp, operator) = match current_token {
                Token::Operator(op) => {
                    let (lbp, rbp) = op.get_bp();
                    (lbp, rbp, op.clone())
                },
                _ => break,
            };

            if lbp < min_bp {
                break;
            }

            self.advance();
            let rhs = self.parse_expression(rbp)?;
            lhs = ASTNode::BinOp {
                op: operator,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            };
        }

        Ok(lhs)
    }

    fn parse_let(&mut self) -> Result<ASTNode<'a>, ParseError<'a>> {
        self.advance(); // skip 'let'
        let mut var_mut = false;

        if let Some((Token::Keyword("mut"), _)) = self.view(0) {
            var_mut = true;
            self.advance();
        }

        let var_name = match self.view(0) {
            Some((Token::Identifier(name), _)) => *name,
            Some((other, span)) => {
                return Err(ParseError {
                    name: ErrorCode::E1002,
                    msg: format!("Expected identifier, found {}", other),
                    place: span.clone(),
                    src: self.source,
                    help: "".to_string()
                });
            }
            None => return Err(self.handle_err_eof(Some(Token::Identifier("")))),
        };

        self.advance();
        
        let var_type = if let Some((Token::Colon, _)) = self.view(0) {
            self.advance();
            match self.view(0).cloned() {
                Some((Token::Identifier(type_name), _)) => {
                    self.advance();
                    let resolved = self.aliases.resolve(type_name);
                    if resolved.is_some() {
                        resolved
                    } else {
                        return Err(ParseError {
                            name: ErrorCode::E1002,
                            msg: format!("Unknown type alias: {}", type_name),
                            place: Span { line: 0, column: 0, startpos: 0, endpos: 0 },
                            src: self.source,
                            help: "".to_string()
                        })
                    }
                }
                _ => return Err(ParseError {
                    name: ErrorCode::E1002,
                    msg: "Expected type identifier after colon".to_string(),
                    place: Span { line: 0, column: 0, startpos: 0, endpos: 0 },
                    src: self.source,
                    help: "".to_string()
                }),
            }
        } else {
            None
        };

        let initializer = if let Some((Token::Operator(Op::Equal), _)) = self.view(0) {
            self.advance();
            Some(self.parse_expression(0)?)
        } else {
            None
        };

        self.consume(Token::Semicolon)?;

        Ok(ASTNode::VarDecl { 
            var_name, 
            var_mut, 
            var_type, 
            initializer: Box::new(initializer),
        })
    }

    fn parse_block(&mut self) -> Result<Vec<ASTNode<'a>>, ParseError<'a>> {
        self.advance();
        let mut stmts: Vec<ASTNode<'a>> = Vec::new();
        while let Some((current_token, _)) = self.view(0).cloned() {
            if current_token == Token::Keyword("end") || current_token == Token::Keyword("else") {
                break
            }
            let result = self.parse_statement()?;
            //println!("{:?}", result);
            stmts.push(result)
        }

        Ok(stmts)
    }

    fn parse_if(&mut self) -> Result<ASTNode<'a>, ParseError<'a>> {
        self.advance(); // skip 'if'
        let condition = self.parse_expression(0)?;

        let (then_body, if_stat) = if let Some((current_token, _)) = self.view(0).cloned() {
            match current_token {
                Token::Keyword("do") => (self.parse_block()?, "block"),
                _ => (vec![self.parse_statement()?], "statement") // single-statement bodies
            }
        } else {
            return Err(ParseError {
                name: ErrorCode::E1000,
                msg: "Unexpected end of input, expected statement or block".to_string(),
                place: self.handle_err_eof(None).place,
                src: self.source,
                help: "".to_string()
            })
        };

        let else_body = if let Some((current_token, _)) = self.view(0).cloned() {
            if current_token == Token::Keyword("else") {
                self.advance();
                if let Some((current_token, _)) = self.view(0).cloned() {
                    match current_token {
                        Token::Keyword("do") => {
                            let block = self.parse_block()?;
                            self.consume(Token::Keyword("end"))?;
                            block
                        },
                        _ => vec![self.parse_statement()?] // single-statement bodies
                    }
                } else {
                    return Err(ParseError {
                        name: ErrorCode::E1000,
                        msg: "Unexpected end of input, expected statement or block".to_string(),
                        place: self.handle_err_eof(None).place,
                        src: self.source,
                        help: "".to_string()
                    })
                }
            } else {
                if if_stat == "block" {
                    self.consume(Token::Keyword("end"))?;
                }
                vec![]
            }
        } else {
            return Ok(ASTNode::IfStatement {
                condition: Box::new(condition), then_body: then_body, else_body: vec![]
            })
        };

        //println!("{:#?}, {:#?}, {:#?}", condition, then_body, else_body);

        Ok(ASTNode::IfStatement {
            condition: Box::new(condition), then_body: then_body, else_body: else_body
        })
    }

    fn parse_while(&mut self) -> Result<ASTNode<'a>, ParseError<'a>> {
        self.advance(); // skip 'while'
        let condition = self.parse_expression(0)?;
        if let Some((current_token, _place)) = self.view(0).cloned() {
            let body = match current_token {
                Token::Keyword("do") => {
                    let stmts = self.parse_block()?;
                    self.consume(Token::Keyword("end"))?;
                    stmts
                },
                _ => {
                    let res = self.parse_expression(0)?;
                    self.consume(Token::Semicolon)?;
                    vec![res]
                }
            };
            Ok(ASTNode::WhileLoop {
                condition: Box::new(condition),
                body: body
            })

        } else {
            Err(ParseError {
                name: ErrorCode::E1000,
                msg: "Unexpected end of input, expected statement or block".to_string(),
                place: self.handle_err_eof(None).place,
                src: self.source,
                help: "".to_string()
            })
        }
    }

    fn parse_statement(&mut self) -> Result<ASTNode<'a>, ParseError<'a>> {
        if let Some((current_token, _place)) = self.view(0).cloned() {
            //println!("{:?}", current_token);
            let is_let = matches!(current_token, Token::Keyword("let"));
            let is_if = matches!(current_token, Token::Keyword("if"));
            let is_while = matches!(current_token, Token::Keyword("while"));
            
            if is_let {
                Ok(self.parse_let()?)
            } else if is_if {
                Ok(self.parse_if()?)
            } else if is_while {
                Ok(self.parse_while()?)
            } else {
                let res = self.parse_expression(0)?;
                //println!("{:#?}", res);
                self.consume(Token::Semicolon)?;
                Ok(res)
            }
        } else {
            Err(ParseError {
                name: ErrorCode::E1000,
                msg: "Unexpected end of input, expected statement".to_string(),
                place: self.handle_err_eof(None).place,
                src: self.source,
                help: "".to_string()
            })
        }
    }

    pub fn parse_program(&mut self) -> (ASTNode<'a>, Vec<ParseError<'a>>) {
        let mut stmts: Vec<ASTNode<'a>> = Vec::new();
        let mut errors: Vec<ParseError> = Vec::new();
        
        while let Some((_t, place)) = self.view(0).cloned() {
            //println!("{:#?}", t);
            let result = self.parse_statement();
            //println!("{:?}", result);
            match result.clone() {
                Ok(node) => {
                    let check_result = self.type_check(node.clone(), place);
                    //println!("{:#?}", check_result);
                    if check_result.is_err() { errors.push(check_result.clone().unwrap_err()) }
                    stmts.push(node);
                },
                Err(err) => errors.push(err)
            }
            //println!("{:#?}", result);
        }

        (ASTNode::Root(stmts), errors)
    }

    fn handle_err_eof(&self, expected: Option<Token>) -> ParseError<'a> {
        // Find the last valid token
        let last_span = if self.pos > 0 {
            self.toks.get(self.pos - 1).map(|(_, span)| span.clone())
        } else if !self.toks.is_empty() {
            self.toks.get(0).map(|(_, span)| span.clone())
        } else {
            None
        };

        let span = last_span.unwrap_or_else(|| Span { line: 0, column: 0, startpos: 0, endpos: 0 });
        
        match expected {
            Some(expected_token) => ParseError {
                name: ErrorCode::E1000,
                msg: format!("Unexpected end of input, expected {}", expected_token),
                place: span,
                src: self.source,
                help: "".to_string()
            },
            None => ParseError {
                name: ErrorCode::E1000,
                msg: "Unexpected end of input".to_string(),
                place: span,
                src: self.source,
                help: "".to_string()
            },
        }
    }

    #[allow(dead_code)]
    fn handle_err_eof_multiple(&self, expected: Option<Vec<Token>>) -> ParseError<'a> {
        // Find the last valid token
        let last_span = if self.pos > 0 {
            self.toks.get(self.pos - 1).map(|(_, span)| span.clone())
        } else if !self.toks.is_empty() {
            self.toks.get(0).map(|(_, span)| span.clone())
        } else {
            None
        };

        let span = last_span.unwrap_or_else(|| Span { line: 0, column: 0, startpos: 0, endpos: 0 });
        
        match expected {
            Some(expected_tokens) => ParseError {
                name: ErrorCode::E1000,
                msg: format!("Unexpected end of input, expected one of {}", 
                    expected_tokens.iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")),
                place: span,
                src: self.source,
                help: "".to_string()
            },
            None => ParseError {
                name: ErrorCode::E1000,
                msg: "Unexpected end of input".to_string(),
                place: span,
                src: self.source,
                help: "".to_string()
            },
        }
    }

    fn type_check(&mut self, node: ASTNode<'a>, place: Span) -> Result<Option<Type<'a>>, ParseError<'a>> {
        //println!("{:#?}", self.env);
        //println!("this function was called on node: {:#?}", node);
        match node {
            ASTNode::Root(_) => unreachable!("how tf did this root get here"),
            ASTNode::IntLit(_i) => Ok(Some(Type::Int)),
            ASTNode::FloatLit(_i) => Ok(Some(Type::Float)),
            ASTNode::Bool(_i) => Ok(Some(Type::Bool)),
            ASTNode::Identifier(i) => {
                let mut rev_env = self.env.clone();
                rev_env.reverse();
                for scope in rev_env.clone() {
                    for symbol in scope.table {
                        if symbol.name == i {
                            return Ok(symbol.symbol_type)
                        }
                    }
                }

                let mut available_names: Vec<String> = Vec::new();
                for scope in rev_env {
                    available_names.extend(
                        scope.table
                        .iter()
                        .map(|s| s.name.clone())
                        .collect::<Vec<_>>()
                    );
                }
                //println!("{:?}", available_names);

                Err(ParseError {
                    name: ErrorCode::E1003,
                    msg: format!("Undefined identifier: {}", i),
                    place: place,
                    src: self.source,
                    help: if let Some(sugg) = suggest_similar_name(i, &available_names) {
                        format!("Did you mean: {}?", format!("'{}'", sugg).cyan())
                    } else {
                        "".to_string()
                    }
                })
            },
            ASTNode::VarDecl {
                var_name,
                var_mut,
                var_type,
                initializer: _
            } => {
                self.env
                .last_mut()
                .unwrap()
                .table
                .push(Symbol {
                    name: var_name.to_string(),
                    symbol_type: var_type,
                    mutable: var_mut,
                    defined_at: place
                });
                Ok(None)
            },
            ASTNode::IfStatement {
                condition,
                then_body,
                else_body
            } => {
                let condition_type = self.type_check(*condition, place)?;
                if condition_type != Some(Type::Bool) {
                    return Err(ParseError {
                        name: ErrorCode::E1007,
                        msg: "Expected boolean expression".to_string(),
                        place: place,
                        src: self.source,
                        help: "".to_string()
                    })
                }
                let mut then_body_type: Option<Type<'a>> = None;
                let mut else_body_type: Option<Type<'a>> = None;

                for then_node in then_body {
                    then_body_type = self.type_check(then_node, place)?;
                }
                for else_node in else_body {
                    else_body_type = self.type_check(else_node, place)?;
                }

                if (then_body_type == else_body_type) || ((then_body_type == None) ^ (else_body_type == None)) {
                    return Ok(then_body_type)
                } else {
                    match (then_body_type, else_body_type) {
                        (Some(type_), None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Then body and else body have different types: type {} and type '()'", 
                            type_
                        ),
                        place: place,
                        src: self.source,
                        help: "".to_string()
                        }),
                        (None, Some(type_)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Then body and else body have different types: type '()' and type {}", 
                            type_
                        ),
                        place: place,
                        src: self.source,
                        help: "".to_string()
                        }),
                        _ => unreachable!()
                    }
                }
            },
            ASTNode::WhileLoop {
                condition,
                body
            } => {
                let condition_type = self.type_check(*condition, place)?;
                if condition_type != Some(Type::Bool) {
                    return Err(ParseError {
                        name: ErrorCode::E1007,
                        msg: "Expected boolean expression".to_string(),
                        place: place,
                        src: self.source,
                        help: "".to_string()
                    })
                }
                let mut body_type: Option<Type<'a>> = None;
                for node in body {
                    body_type = self.type_check(node, place)?;
                }

                Ok(body_type)
            },
            ASTNode::BinOp {
                op,
                lhs,
                rhs
            } => {
                match op {
                    Op::Plus => {
                        let lhs_type = self.type_check(*lhs.clone(), place)?;
                        let rhs_type = self.type_check(*rhs.clone(), place)?;
                        match (lhs_type, rhs_type) {
                        (Some(Type::Int), Some(Type::Int)) => Ok(Some(Type::Int)),
                        (Some(Type::Float), Some(Type::Float)) => Ok(Some(Type::Float)),
                        (Some(l), Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '+' operation on type {} and type {}", l, r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '+' operation on type '()' and type '()'"),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (Some(l), None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '+' operation on type {} and type '()'", l),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '+' operation on type '()' and type {}", r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                    }},
                    Op::Minus => {
                        let lhs_type = self.type_check(*lhs.clone(), place)?;
                        let rhs_type = self.type_check(*rhs.clone(), place)?;
                        match (lhs_type, rhs_type) {
                        (Some(Type::Int), Some(Type::Int)) => Ok(Some(Type::Int)),
                        (Some(Type::Float), Some(Type::Float)) => Ok(Some(Type::Float)),
                        (Some(l), Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '-' operation on type {} and type {}", l, r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '-' operation on type '()' and type '()'"),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (Some(l), None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '-' operation on type {} and type '()'", l),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '-' operation on type '()' and type {}", r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                    }},
                    Op::Star => {
                        let lhs_type = self.type_check(*lhs.clone(), place)?;
                        let rhs_type = self.type_check(*rhs.clone(), place)?;
                        match (lhs_type, rhs_type) {
                        (Some(Type::Int), Some(Type::Int)) => Ok(Some(Type::Int)),
                        (Some(Type::Float), Some(Type::Float)) => Ok(Some(Type::Float)),
                        (Some(l), Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '*' operation on type {} and type {}", l, r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '*' operation on type '()' and type '()'"),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (Some(l), None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '*' operation on type {} and type '()'", l),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '*' operation on type '()' and type {}", r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                    }},
                    Op::Slash => {
                        let lhs_type = self.type_check(*lhs.clone(), place)?;
                        let rhs_type = self.type_check(*rhs.clone(), place)?;
                        if *rhs == ASTNode::IntLit(0) || *rhs == ASTNode::FloatLit(0.0) {
                            return Err(ParseError {
                                name: ErrorCode::E1005,
                                msg: "Cannot divide by 0".to_string(),
                                place: place,
                                src: self.source,
                                help: "".to_string()
                            })
                        }
                        match (lhs_type, rhs_type) {
                        (Some(Type::Int), Some(Type::Int)) => Ok(Some(Type::Int)),
                        (Some(Type::Float), Some(Type::Float)) => Ok(Some(Type::Float)),
                        (Some(l), Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '/' operation on type {} and type {}", l, r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '/' operation on type '()' and type '()'"),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (Some(l), None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '/' operation on type {} and type '()'", l),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '/' operation on type '()' and type {}", r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                    }},
                    Op::Equal => {
                        let lhs_type = self.type_check(*lhs.clone(), place)?;
                        let rhs_type = self.type_check(*rhs.clone(), place)?;

                        if let ASTNode::Identifier(n) = *lhs {
                            if lhs_type == rhs_type {
                                let mut rev_env = self.env.clone();
                                rev_env.reverse();
                                for scope in rev_env {
                                    for symbol in scope.table {
                                        if symbol.name == n {
                                            if symbol.mutable {
                                                return Ok(None)
                                            } else {
                                                return Err(ParseError {
                                                    name: ErrorCode::E1006,
                                                    msg: format!("Immutable variable '{}' cannot be mutated", n),
                                                    place: place,
                                                    src: self.source,
                                                    help: format!("{}{}",
                                                        "Variable was defined here:\n".cyan().bold(),
                                                        symbol
                                                            .get_message(self.source, true)
                                                    )
                                                    
                                                })
                                            }
                                        }
                                    }
                                }

                                Err(ParseError {
                                    name: ErrorCode::E1003,
                                    msg: format!("Undefined identifier: {}", n),
                                    place: place,
                                    src: self.source,
                                    help: "".to_string()
                                })
                            } else {
                                match (lhs_type, rhs_type) {
                                    (Some(l), Some(r)) => Err(ParseError {
                                        name: ErrorCode::E1007,
                                        msg: format!("Expected type {}, found type {}", l, r),
                                        place: place,
                                        src: self.source,
                                        help: "".to_string()
                                    }),
                                    (None, None) => unreachable!("yo i found '()' and '()' check the src code"), // risky, idk how to get to this yet...
                                    (Some(l), None) => Err(ParseError {
                                        name: ErrorCode::E1007,
                                        msg: format!("Expected type {}, found type '()'", l),
                                        place: place,
                                        src: self.source,
                                        help: "".to_string()
                                    }),
                                    (None, Some(r)) => unreachable!("yo i found '()' and '{}' check the src code", r), // risky, idk how to get to this yet...
                                }
                            }
                        } else {
                            Err(ParseError {
                                name: ErrorCode::E1002,
                                msg: "Expected identifier".to_string(),
                                place: place,
                                src: self.source,
                                help: "".to_string()
                            })
                        }
                    },
                    Op::CmpEq => {
                        Ok(Some(Type::Bool))
                    },
                    Op::CmpGt => {
                        let lhs_type = self.type_check(*lhs.clone(), place)?;
                        let rhs_type = self.type_check(*rhs.clone(), place)?;
                        match (lhs_type, rhs_type) {
                        (Some(Type::Int), Some(Type::Int)) => Ok(Some(Type::Bool)),
                        (Some(Type::Float), Some(Type::Float)) => Ok(Some(Type::Bool)),
                        (Some(l), Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '>' operation on type {} and type {}", l, r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '>' operation on type '()' and type '()'"),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (Some(l), None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '>' operation on type {} and type '()'", l),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '>' operation on type '()' and type {}", r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                    }},
                    Op::CmpLt => {
                        let lhs_type = self.type_check(*lhs.clone(), place)?;
                        let rhs_type = self.type_check(*rhs.clone(), place)?;
                        match (lhs_type, rhs_type) {
                        (Some(Type::Int), Some(Type::Int)) => Ok(Some(Type::Bool)),
                        (Some(Type::Float), Some(Type::Float)) => Ok(Some(Type::Bool)),
                        (Some(l), Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '<' operation on type {} and type {}", l, r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '<' operation on type '()' and type '()'"),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (Some(l), None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '<' operation on type {} and type '()'", l),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '<' operation on type '()' and type {}", r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                    }},
                    Op::CmpGe => {
                        let lhs_type = self.type_check(*lhs.clone(), place)?;
                        let rhs_type = self.type_check(*rhs.clone(), place)?;
                        match (lhs_type, rhs_type) {
                        (Some(Type::Int), Some(Type::Int)) => Ok(Some(Type::Bool)),
                        (Some(Type::Float), Some(Type::Float)) => Ok(Some(Type::Bool)),
                        (Some(l), Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '>=' operation on type {} and type {}", l, r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '>=' operation on type '()' and type '()'"),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (Some(l), None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '>=' operation on type {} and type '()'", l),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '>=' operation on type '()' and type {}", r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                    }},
                    Op::CmpLe => {
                        let lhs_type = self.type_check(*lhs.clone(), place)?;
                        let rhs_type = self.type_check(*rhs.clone(), place)?;
                        match (lhs_type, rhs_type) {
                        (Some(Type::Int), Some(Type::Int)) => Ok(Some(Type::Bool)),
                        (Some(Type::Float), Some(Type::Float)) => Ok(Some(Type::Bool)),
                        (Some(l), Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '<=' operation on type {} and type {}", l, r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '<=' operation on type '()' and type '()'"),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (Some(l), None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '<=' operation on type {} and type '()'", l),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '<=' operation on type '()' and type {}", r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                    }},
                    Op::CmpNe => {
                        Ok(Some(Type::Bool))
                    },
                    Op::Concat => {
                        let lhs_type = self.type_check(*lhs.clone(), place)?;
                        let rhs_type = self.type_check(*rhs.clone(), place)?;
                        match (lhs_type, rhs_type) {
                        (Some(Type::Int), Some(Type::Int)) => Ok(Some(Type::Str)),
                        (Some(l), Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '++' operation on type {} and type {}", l, r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '++' operation on type '()' and type '()'"),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (Some(l), None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '++' operation on type {} and type '()'", l),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '++' operation on type '()' and type {}", r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                    }},
                    Op::BitwiseOr | Op::BitwiseAnd | Op::BitwiseXor => {
                        let lhs_type = self.type_check(*lhs.clone(), place)?;
                        let rhs_type = self.type_check(*rhs.clone(), place)?;
                        match (lhs_type, rhs_type) {
                        (Some(Type::Bool), Some(Type::Bool)) => Ok(Some(Type::Bool)),
                        (Some(l), Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '{}' operation on type {} and type {}", op.to_string(), l, r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '{}' operation on type '()' and type '()'", op.to_string()),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (Some(l), None) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '{}' operation on type {} and type '()'", op.to_string(), l),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                        (None, Some(r)) => Err(ParseError {
                            name: ErrorCode::E1004,
                            msg: format!("Cannot do '{}' operation on type '()' and type {}", op.to_string(), r),
                            place: place,
                            src: self.source,
                            help: "".to_string()
                        }),
                    }},
                }
            }
        }
    }
}