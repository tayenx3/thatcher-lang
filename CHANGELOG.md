# Changelog  
> Track the evolution of the Thatcher programming language

## [v0.1.0-alpha] - September 21st, 2025

### Added
- **Turing completeness** - Thatcher can now compute anything!
- **3 primitive data types**:
  - `Int` - Integer numbers
  - `Float` - Floating-point numbers
  - `Bool` - Boolean values (`true`/`false`)
- **13 operators**:
  - Arithmetic: `+`, `-`, `*`, `/`
  - Comparison: `==`, `!=`, `>`, `<`, `>=`, `<=`
  - Logical: `&&` (AND), `||` (OR), `^^` (XOR)
- **Control flow**:
  - `if`/`else` conditional statements  
  - `while` loops
- **Variable system**:
  - Type annotations: `let x: Int = 5;`
  - Mutability: `let mut x = 10;`
- **Basic I/O** via Erlang's `io:format`

### Examples
#### Countdown Program
```thatcher
let mut x: Int = 10;

while x > 0 do
    x + 0;  // Print current value
    x = x - 1;
end
```

#### Logical Operations Demo
```thatcher
let bol1: Bool = true;
let bol2: Bool = false;

if bol1 || bol2 1 + 0;  // Prints 1 (OR)
if bol1 && bol2 2 + 0;  // No output (AND)
if bol1 ^^ bol2 3 + 0;  // Prints 3 (XOR)
```

### Compiling
```bash
cargo run <filename>.tc
```

### Notes
- This initial release proves Turing completeness

- The language can implement complex algorithms and logic

- Foundation for future features (strings, functions, modules)

- Feedback and contributions are welcome!