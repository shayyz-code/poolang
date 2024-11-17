# PooLang

This is my tiny interpreted language written in Rust, featuring variable declarations, arithmetic operations, conditional statements, and control flow. This project includes a lexer, parser, and interpreter. The name `Poo` originates from Guinea Pig translated from `Burmese`.

## Crafted by **Shayy**

## 📜 Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Syntax Overview](#syntax-overview)
- [Example Code](#example-code)
- [Development](#development)
- [Contributing](#contributing)

## ✨ Features

- **Arithmetic Expressions**: Supports addition, subtraction, multiplication, and division with correct operator precedence.
- **Variable Declarations**: Uses `poo` keyword for variable declarations.
- **Mutable Variables**: Like in Rust, all variables are immutable by default. Uses `mut` for mutable variables.
- **Conditional Statements**: Includes `if`, `else`, and `elif` for branching.
- **Control Flow**: Supports `while` loops and `return` statements.
- **Custom Operators**:
  - Assignment operator: `<<`
  - Open scope operator: `>>`
- **Lexer, Parser, and Interpreter**: A full pipeline from tokenizing source code to executing it.

## 🚀 Installation

### Prerequisites

- Rust and Cargo installed. If you don't have them installed, follow the [Rust installation guide](https://www.rust-lang.org/tools/install).

### Steps

1. Clone the repository:

   ```bash
   git clone https://github.com/shayyz-code/poolang.git
   cd poolang
   ```

2. Build the project:

   ```bash
   cargo build
   ```

3. Run tests:
   ```bash
   cargo test
   ```

## 🛠️ Usage

You can use the interpreter to run files containing your custom language code.

### Running the Interpreter

To run the interpreter on a source file:

```bash
cargo run <path_to_your_source_file>
```

Example:

```bash
cargo run examples/test.poo
```

## 📝 Syntax Overview

The language features basic syntax for arithmetic, variable declarations, and control flow:

### **Variable Declarations**

```poo
poo x << 10;
poo y << 5 + 2 * 3;
```

### **Arithmetic Operations**

```poo
poo result << x + y * 2 - 10 / 2;
```

### **Conditional Statements**

```poo
if x > y >>
    return x;
else >>
    return y;
```

### **Loops**

```poo
poo mut count << 0;
while count < 10 >>
    count << count + 1;
```

### **Return Statement**

```poo
poo value << 42;
return value;
```

## 📚 Example Code

Here is a sample program in this language:

```poo
poo x << 10;
poo y << 5 + 2 * 3;
poo mut result << 0;

if x > y >>
    result << x - y;
elif x < y >>
    result << y - x;
else >>
    result << 0;

while result < 50 >>
    result << result + 5;

return result;
```

### Expected Output

```
Result: 50
```

## 🛠️ Development

### Project Structure

```
.
├── src
│   ├── lexer.rs         # Lexical analysis (tokenizer)
│   ├── parser.rs        # Parsing logic
│   ├── interpreter.rs   # Interpreter for executing code
│   ├── ast.rs           # Abstract Syntax Tree (AST) definitions
│   └── main.rs          # Entry point
├── examples
│   └── test.poo         # Sample code
└── Cargo.toml           # Project configuration
```

### Running in Debug Mode

To run the interpreter with detailed output for debugging:

```bash
RUST_LOG=debug cargo run examples/test.poo
```

### Testing

To run all tests:

```bash
cargo test
```

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request or open an Issue if you find a bug or have a feature request.

### Steps to Contribute

1. Fork the repository.
2. Create a new branch:
   ```bash
   git checkout -b feature/your-feature-name
   ```
3. Make your changes and commit them:
   ```bash
   git commit -m "Add your feature description"
   ```
4. Push to the branch:
   ```bash
   git push origin feature/your-feature-name
   ```
5. Open a Pull Request.

## 💬 Feedback

If you have any questions or feedback, feel free to reach out or open an issue in the repository.

---

Happy Coding! 🎉

---
