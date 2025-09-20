use crate::global::*;
use std::fmt;

#[allow(dead_code)]
#[derive(Debug)]
pub struct ParseError<'a> {
    pub name: ErrorCode,
    pub msg: String,
    pub place: Span, // Changed to owned Span instead of reference
    pub src: &'a str,
}

impl fmt::Display for ParseError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.get_message(self.name.clone(), &self.msg, self.place.clone(), self.src))
    }
}

impl ErrorMsg for ParseError<'_> {}

pub struct ThatchParser<'a> {
    pos: usize,
    toks: Vec<(Token<'a>, Span)>,
    source: &'a str,
}

impl<'a> ThatchParser<'a> {
    pub fn new() -> Self {
        ThatchParser {
            pos: 0,
            toks: Vec::new(),
            source: "",
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
        if let Some((current_token, place)) = self.view(0) {
            if current_token == &expected {
                self.advance();
                Ok(())
            } else {
                Err(ParseError {
                    name: ErrorCode::E1002,
                    msg: format!("Expected {}, found {}", expected, current_token),
                    place: place.clone(),
                    src: self.source,
                })
            }
        } else {
            let mut offset: i32 = -1;
            while self.view(offset).is_none() {
                offset -= 1;
            }
            let (_, span) = self.view(offset).unwrap();

            Err(ParseError {
                name: ErrorCode::E1000,
                msg: format!("Unexpected end of input, expected {}", expected),
                place: span.clone(),
                src: self.source,
            })
        }
    }

    fn parse_primary(&mut self) -> Result<ASTNode<'a>, ParseError<'a>> {
        let (current_token, place) = match self.view(0) {
            Some((token, span)) => (token.clone(), span.clone()),
            None => {
                let mut offset: i32 = -1;
                while self.view(offset).is_none() {
                    offset -= 1;
                }
                let (_, span) = self.view(offset).unwrap();
                
                return Err(ParseError {
                    name: ErrorCode::E1000,
                    msg: "Unexpected end of input, expected expression".to_string(),
                    place: span.clone(),
                    src: self.source,
                });
            }
        };

        let node = match current_token {
            Token::Int(number) => {
                self.advance();
                ASTNode::IntLit(number)
            },
            Token::Float(number) => {
                self.advance();
                ASTNode::FloatLit(number)
            },
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
            // Add other token variants as needed
            _ => return Err(ParseError {
                name: ErrorCode::E1001,
                msg: format!("Unexpected {}", current_token),
                place: place.clone(), // Use the cloned place
                src: self.source,
            }),
        };

        Ok(node)
    }

    fn parse_expression(&mut self, min_bp: i32) -> Result<ASTNode<'a>, ParseError<'a>> {
        let mut lhs = self.parse_primary()?;
        
        while let Some((current_token, _)) = self.view(0) {
            let op_info = match current_token {
                Token::Operator(op) => {
                    let (lbp, rbp) = op.get_bp();
                    (lbp, rbp, op.clone()) // Clone the operator here
                },
                _ => break,
            };

            let (lbp, rbp, operator) = op_info;

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

    #[allow(suspicious_double_ref_op)]
    fn parse_let(&mut self) -> Result<ASTNode<'a>, ParseError<'a>> {
        self.advance(); // skip 'let'
        let mut var_mut = false;

        if let Some((Token::Keyword("mut"), _place)) = self.view(0) {
            var_mut = true;
            self.advance()
        }

        let (var_name, _var_span) = match self.view(0) {
            Some((Token::Identifier(name), span)) => (name.clone(), span.clone()),
            Some((other, span)) => {
                return Err(ParseError {
                    name: ErrorCode::E1002,
                    msg: format!("Expected identifier, found {}", other),
                    place: span.clone(),
                    src: self.source,
                });
            }
            None => {
                let mut offset: i32 = -1;
                while self.view(offset).is_none() {
                    offset -= 1;
                }
                let (_, span) = self.view(offset).unwrap();
                
                return Err(ParseError {
                    name: ErrorCode::E1000,
                    msg: "Unexpected end of input, expected identifier".to_string(),
                    place: span.clone(),
                    src: self.source,
                });
            }
        };

        self.advance();
        let mut initializer = None;
        
        match self.view(0) {
            Some((Token::Operator(Op::Equal), _)) => {
                self.advance();
                initializer = Some(self.parse_expression(0)?);
            }, // Only valid choices
            Some((Token::Semicolon, _)) => {},
            Some((other, span)) => {
                return Err(ParseError {
                    name: ErrorCode::E1002,
                    msg: format!("Expected identifier, found {}", other),
                    place: span.clone(),
                    src: self.source,
                });
            }
            None => {
                let mut offset: i32 = -1;
                while self.view(offset).is_none() {
                    offset -= 1;
                }
                let (_, span) = self.view(offset).unwrap();
                
                return Err(ParseError {
                    name: ErrorCode::E1000,
                    msg: "Unexpected end of input, expected '='".to_string(),
                    place: span.clone(),
                    src: self.source,
                });
            }
        };

        self.consume(Token::Semicolon)?;

        Ok(ASTNode::VarDecl { var_name: var_name, var_mut, var_type: "" , initializer: Box::new(initializer) })
    }

    pub fn parse_program(&mut self) -> Result<ASTNode<'a>, ParseError<'a>> {
        let mut stmts: Vec<ASTNode<'a>> = Vec::new();
        
        while self.view(0).is_some() {
            let node = match self.view(0) {
                Some((Token::Keyword("let"), _)) => self.parse_let()?,
                _ => {
                    let res = self.parse_expression(0)?;
                    self.consume(Token::Semicolon)?;
                    res
                }
            };
            stmts.push(node);
        }

        Ok(ASTNode::Root(stmts))
    }
}