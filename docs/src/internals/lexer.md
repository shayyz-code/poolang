# Lexer

The Lexer (also known as a Tokenizer) is the first stage of the PooLang pipeline. Its job is to take raw source code (a string of characters) and turn it into a sequence of meaningful **Tokens**.

## How it works

The Lexer scans the input character by character and groups them into tokens like:
- **Keywords**: `poo`, `mut`, `if`, `while`
- **Operators**: `<<`, `+`, `-`, `*`, `/`
- **Literals**: Integers (`10`), Strings (`"hi"`), Booleans (`true`)
- **Identifiers**: Variable and function names

## Implementation

The implementation is located in `src/lexer.rs`. It uses a simple state-machine approach to recognize different patterns of text.

### Example

Input:
```poo
poo x << 5;
```

Tokens produced:
1. `Keyword(Poo)`
2. `Identifier("x")`
3. `Operator(Assign)`
4. `Literal(Integer(5))`
5. `Semicolon`
