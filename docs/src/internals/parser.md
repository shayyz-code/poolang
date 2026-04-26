# Parser

The Parser takes the stream of tokens produced by the Lexer and organizes them into a hierarchical structure called an **Abstract Syntax Tree (AST)**.

## How it works

PooLang uses a **Recursive Descent** parser. It looks at the current token and decides which rule of the language grammar to apply next.

For example, if the parser see the `poo` token, it knows it is starting a variable declaration and expects an identifier followed by an assignment operator.

## Abstract Syntax Tree (AST)

The AST is a tree-like representation of your code. For example, the expression `1 + 2 * 3` would be parsed into a tree where `+` is the root, `1` is the left child, and another tree representing `2 * 3` is the right child.

The AST node definitions can be found in `src/ast.rs`.

## Implementation

The parser logic resides in `src/parser.rs`. It handles:
- Statement parsing
- Expression parsing (with operator precedence)
- Error reporting for syntax errors
