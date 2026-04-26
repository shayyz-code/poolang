# Introduction

**PooLang** is a tiny, interpreted language written in Rust. It was designed to be a lightweight, pedagogical language featuring a full pipeline: lexical analysis, parsing into an Abstract Syntax Tree (AST), and tree-walking interpretation.

The name **Poo** is inspired by the Burmese word for Guinea Pig, reflecting the language's friendly and compact nature.

## Core Philosophies

1. **Simplicity**: Minimal keywords and straightforward syntax.
2. **Rust-Inspired**: Familiar concepts like immutability by default and `mut` for mutability.
3. **Transparent Pipeline**: Designed to be easy to study for those interested in how interpreters work.

## Features at a Glance

- **Explicit Variables**: Uses the `poo` keyword.
- **Assignment**: Uses the unique `<<` operator.
- **Control Flow**: Robust support for `if/elif/else`, `while`, and `for` loops.
- **Functions**: Define reusable logic with `poof`.
