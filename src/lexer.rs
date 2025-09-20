use crate::global::*;

fn str_is_alphanumeric(string: &str) -> bool {
    string.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
}

pub fn classify(string: &str) -> Token {
    match string {
        "let" | "mut" | "Int" | "Float" | "Str" | "Bool" => Token::Keyword(string),
        "+" => Token::Operator(Op::Plus),
        "-" => Token::Operator(Op::Minus),
        "*" => Token::Operator(Op::Star),
        "/" => Token::Operator(Op::Slash),
        "(" => Token::LParen,
        ")" => Token::RParen,
        ";" => Token::Semicolon,
        "=" => Token::Operator(Op::Equal),
        s => {
            if let Ok(int_val) = s.parse::<i64>() {
                Token::Int(int_val)
            } else if let Ok(float_val) = s.parse::<f64>() {
                Token::Float(float_val)
            } else if str_is_alphanumeric(s) {
                Token::Identifier(s)
            } else {
                Token::Unknown(s)
            }
        }
    }
}

pub fn tokenize(program: &String) -> Result<Vec<(Token, Span)>, String> {
    let chars: Vec<char> = program.chars().collect();
    let mut toks: Vec<(Token, Span)> = Vec::new();
    let mut skip: i32 = 0;

    let mut line: usize = 1;
    let mut column: usize = 1;
    let mut startpos: usize = 0;
    let mut endpos: usize = 0;

    let mut curr: String = String::new();

    for ch in chars {
        if skip > 0 {
            skip -= 1;
            continue
        }
        if ch == '\n' {
            line += 1;
            column = 1;
            endpos += 1;
            continue
        }

        match ch {
            '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => { // Alphanumeric
                if curr.is_empty() {
                    startpos = endpos
                }
                curr.push(ch)
            },
            _ if "+-*/();=".contains(ch) => { // Operators/Separators
                if !curr.is_empty() {
                    endpos -= 1;
                    let prev = column;
                    column = startpos;

                    toks.push((classify(&program[startpos..=endpos]), Span { line, column, startpos, endpos}));

                    endpos += 1;
                    column = prev;
                    curr.clear()
                }
                startpos = endpos;
                toks.push((classify(&program[startpos..=endpos]), Span { line, column, startpos, endpos}))
            },
            ' ' => if !curr.is_empty() {
                    endpos -= 1;
                    let prev = column;
                    column = startpos;

                    toks.push((classify(&program[startpos..=endpos]), Span { line, column, startpos, endpos}));

                    endpos += 1;
                    column = prev;
                    curr.clear()
                }
            _ => continue
        }

        endpos += 1;
        column += 1;
    }
    if !curr.is_empty() {
        column -= curr.len();
        toks.push((classify(&program[startpos..=endpos]), Span { line, column, startpos, endpos}))
    }

    Ok(toks)
}