# PooLang

A Tiny Interpreted language written in Rust, featuring variable declarations, arithmetic operations, conditional statements, and control flow. This project includes a lexer, parser, and interpreter. The name `Poo` originates from Guinea Pig translated from `Burmese`.

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
- **Control Flow**: Supports `while` and `for in` loops and `return` statements.
- **Custom Operators**:
  - Assignment operator: `<<`
  - Arrow operator: `>>`
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
cargo run app.poo
```

## 📝 Syntax Overview

The language features basic syntax for arithmetic, variable declarations, and control flow:

### **Variable Declarations**

```poo
poo x << 10;
poo mut y << 5 + 2 * 3;
```

### **Arithmetic Operations**

```poo
poo result << x + y * 2 - 10 / 2;
```

### **Conditional Statements**

```poo
if x > y {
    return x;
} else {
    return y;
}
```

### **Loops**

```poo
use std::pout;

poo mut count << 0;

while count < 10 {
    count << count + 1;
}

for i in 0..3 {
    pout("Hello, World ", i);
}
```

### **Functions**

```poo
use std::pout;

poof getName () >> string {
    poo name << "Shayy";
    return name;
}

pout(getName());
```

## 📚 Example Code

Here is a sample program in my PooLang:

```poo
use std::pout;

poo a << 5.0 * 1.0 - 1.0 * 3.0;
poo b << 2 / 2;
poo mut d << true;
d << false;

poof getHelloWorld () >> string {
    return "Hello, World!";
}

for i in 0..2 {
    pout("Hello, Poo!", i);
}

pout(getHelloWorld());
```

Expected Output:

```poo
Hello, Poo!0
Hello, Poo!1
Hello, World!
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
├── examples             # Sample code
│   ├── donut.poo
│   └── app.poo
└── Cargo.toml           # Project configuration
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
