# Interpreter

The Interpreter is the final stage of the PooLang pipeline. It takes the **Abstract Syntax Tree (AST)** produced by the Parser and executes the logic it represents.

## How it works

PooLang uses a **Tree-Walking Interpreter**. It traverses the AST recursively, "visiting" each node and performing the corresponding action in the real world (e.g., adding two numbers, updating a variable in memory).

## Environment and State

The interpreter maintains an **Environment** (or Scope) which maps variable names to their current values. When you declare a variable with `poo`, it is added to the current environment.

## Implementation

The interpreter is located in `src/interpreter.rs`. Key features include:
- **Value system**: Defines how integers, floats, and strings are stored in memory.
- **Scope management**: Handles nested scopes for loops and functions.
- **Runtime Errors**: Detects issues like dividing by zero or using an undefined variable.
