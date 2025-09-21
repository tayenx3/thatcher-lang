use crate::global::*;

fn str_is_alphanumeric(string: &str) -> bool {
    string.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

pub fn classify(string: &str) -> Token {
    match string {
        "let" | "mut" | "if" | "else" | "do" | "end" | "while" => Token::Keyword(string),
        "+" => Token::Operator(Op::Plus),
        "-" => Token::Operator(Op::Minus),
        "*" => Token::Operator(Op::Star),
        "/" => Token::Operator(Op::Slash),
        "(" => Token::LParen,
        ")" => Token::RParen,
        ";" => Token::Semicolon,
        ":" => Token::Colon,
        "," => Token::Comma,
        "!" => Token::ExclamationMark,
        "=" => Token::Operator(Op::Equal),
        "==" => Token::Operator(Op::CmpEq),
        "<" => Token::Operator(Op::CmpLt),
        ">" => Token::Operator(Op::CmpGt),
        "<=" => Token::Operator(Op::CmpLe),
        ">=" => Token::Operator(Op::CmpGe),
        "!=" => Token::Operator(Op::CmpNe),
        "++" => Token::Operator(Op::Concat),
        "||" => Token::Operator(Op::BitwiseOr),
        "&&" => Token::Operator(Op::BitwiseAnd),
        "^^" => Token::Operator(Op::BitwiseXor),
        s => {
            if let Ok(int_val) = s.parse::<i64>() {
                Token::Int(int_val)
            } else if let Ok(float_val) = s.parse::<f64>() {
                Token::Float(float_val)
            } else if s == "true" || s == "false" {
                Token::Bool(s == "true")
            } else if str_is_alphanumeric(s) {
                Token::Identifier(s)
            } else {
                Token::Unknown(s)
            }
        }
    }
}

#[allow(unused_assignments)]
pub fn tokenize(program: &str) -> Result<Vec<(Token, Span)>, String> {
    let mut toks = Vec::new();
    let mut chars = program.chars().peekable();
    
    let mut line = 1;
    let mut column = 1;
    let mut pos = 0;

    let mut in_string: bool = false;
    
    while let Some(&ch) = chars.peek() {
        let start_pos = pos;
        let start_line = line;
        let start_column = column;
        
        if in_string {
            if ch == '"' {
                in_string = false;
            }
        }
        match ch {
            // Whitespace
            ' ' | '\t' => {
                chars.next();
                pos += 1;
                column += 1;
                continue;
            }
            
            // Newline
            '\n' => {
                chars.next();
                line += 1;
                column = 1;
                pos += 1;
                continue;
            }
            
            // Alphanumeric identifiers
            'a'..='z' | 'A'..='Z' | '_' => {
                let mut ident = String::new();
                ident.push(ch);
                chars.next();
                pos += 1;
                column += 1;
                
                // Allow digits AFTER the first character
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphanumeric() || c == '_' {
                        ident.push(c);
                        chars.next();
                        pos += 1;
                        column += 1;
                    } else {
                        break;
                    }
                }
                
                let end_pos = pos - 1;
                toks.push((
                    classify(&program[start_pos..=end_pos]),
                    Span {
                        line: start_line,
                        column: start_column,
                        startpos: start_pos,
                        endpos: end_pos,
                    }
                ));
            }
            
            // Numbers
            '0'..='9' => {
                let mut number = String::new();
                let mut is_float = false;
                
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_digit() {
                        number.push(c);
                        chars.next();
                        pos += 1;
                        column += 1;
                    } else if c == '.' && !is_float {
                        is_float = true;
                        number.push(c);
                        chars.next();
                        pos += 1;
                        column += 1;
                    } else {
                        break;
                    }
                }
                
                let end_pos = pos - 1;
                toks.push((
                    classify(&program[start_pos..=end_pos]),
                    Span {
                        line: start_line,
                        column: start_column,
                        startpos: start_pos,
                        endpos: end_pos,
                    }
                ));
            }
            
            // Operators and punctuation
            _ => {
                let mut operator = String::new();
                operator.push(ch);
                chars.next();
                pos += 1;
                column += 1;
                
                // Check for multi-character operators (==, !=, <=, >=)
                if let Some(&next_ch) = chars.peek() {
                    let combined = format!("{}{}", ch, next_ch);
                    if matches!(&combined[..], "==" | "!=" | "<=" | ">=" | "++" | "||" | "&&" | "^^") {
                        operator = combined;
                        chars.next();
                        pos += 1;
                        column += 1;
                    } else if matches!(&combined[..], "//") {
                        while let Some(&next_ch) = chars.peek() {
                            if next_ch == '\n' {
                                break;
                            }
                            chars.next();
                            pos += 1;
                            column += 1;
                        }
                        continue;
                    }
                }
                
                let end_pos = pos - 1;
                toks.push((
                    classify(&program[start_pos..=end_pos]),
                    Span {
                        line: start_line,
                        column: start_column,
                        startpos: start_pos,
                        endpos: end_pos,
                    }
                ));
            }
        }
    }
    
    Ok(toks)
}