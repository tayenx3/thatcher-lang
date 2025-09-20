mod global;
mod lexer;
mod tt_parser;
mod compiler;

use std::fs;
use std::process::Command;
use colored::Colorize;
use clap::Parser as ClapParser;

// CLI structure using clap derive
#[derive(ClapParser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Input .tc file to compile
    input: String,

    /// Enable debug output (tokens, AST, etc.)
    #[arg(short, long)]
    debug: bool,

    /// Output file name (default: same as input but .beam)
    #[arg(short, long)]
    output: Option<String>,
}

fn main() {
    let cli = Cli::parse();
    
    match compile_file(&cli.input, cli.debug, cli.output.as_deref()) {
        Ok(()) => println!("{}", "Compilation process complete with no errors".green().bold()),
        Err(e) => eprintln!("{}: {}", "Error".red().bold(), e)
    }
}

fn compile_file(program: &str, debug: bool, output: Option<&str>) -> Result<(), String> {
    let program_contents = fs::read_to_string(program)
        .map_err(|e| format!("Unable to read file '{}': {}", program, e))?
        .replace("\r", "");

    if debug {
        println!("Attempting to compile contents:\n{}\n", program);
    }

    let tokens = lexer::tokenize(&program_contents)
        .map_err(|e| e.to_string())?; // Convert ParseError to String
    
    if debug {
        println!("Tokens: {:?}\n", tokens);
    }

    if tokens.is_empty() { 
        return Ok(()) 
    }

    let mut parser = tt_parser::ThatchParser::new();
    parser.load(tokens, &program_contents);
    let ast = parser.parse_program()
        .map_err(|e| e.to_string())?; // Convert ParseError to String

    if debug {
        println!("AST: {:#?}", ast);
    }

    // Determine output filename
    let output_filename = output.map(String::from)
        .unwrap_or_else(|| program.replace(".tc", ""));
    
    let mut compiler_ = compiler::Compiler::new();
    compiler_.compile(ast, output_filename.clone())
        .map_err(|e| e.to_string())?; // Convert compiler error to String

    // Compile with erl
    let erl_output: std::process::Output = Command::new("erl")
        .arg("-noshell")
        .arg("-run")
        .arg("compile")
        .arg("file")
        .arg(&output_filename)
        .arg("-s")
        .arg("init")
        .arg("stop")
        .output()
        .map_err(|e| format!("Failed to run erl: {}", e))?;

    if !erl_output.status.success() {
        return Err(format!("Erlang compilation failed: {}", 
            String::from_utf8_lossy(&erl_output.stderr)));
    }

    println!("✅ Successfully compiled to: {}.beam", 
        output_filename
    );

    Ok(())
}

#[allow(dead_code)]
fn handle_extension(string: &str) -> String {
    let mut chars: Vec<char> = string.chars().collect();
    chars.reverse();
    let mut current: String = String::new();
    let mut result: String = String::new();

    let mut pos: usize = 0;
    while pos < chars.len() {
        current.push(chars[pos]);
        
        if current == "ct." {
            pos += 1;
            while pos < chars.len() {
                result.push(chars[pos]);
                pos += 1;
            }
            
            let mut reversed_result: Vec<char> = result.chars().collect();
            reversed_result.reverse();
            return reversed_result.into_iter().collect();
        }
        pos += 1;
    }
    string.to_string()
}