# Introduction

> **This book is being rewritten.** Most pages describe a mix of planned and
> legacy syntax, and many of their examples do not run on the current
> interpreter. Pages marked **Legacy page** will be rewritten in
> [P7 Docs truth](https://github.com/shayyz-code/poolang/milestone/8). Until
> then, trust the example on this page and `examples/app.poo` in the
> repository over the rest of the book.

The name **Poo** comes from the Burmese word for guinea pig (ပူး).

## Where PooLang is today

PooLang v0.1.11 is a small tree-walking interpreter written in Rust with no
third-party dependencies. Source code goes through three stages: a lexer turns
it into tokens, a parser builds an abstract syntax tree, and an interpreter
walks that tree to run the program. The `poo` command runs a `.poo` file.

This program runs on v0.1.11:

```poo
use std::poutln;

// Comments open and close with two slashes. //
poo name <: "Guinea Pig";
mut count int = 0;
count = count + 1;

poof add(a int, b int) >> int {
    return a + b;
}

if count > 0 and not false {
    poutln("Hello, ", name, "! 2 + 3 = ", add(2, 3));
} elif count == 0 {
    poutln("unreachable");
}
```

It prints:

```text
Hello, Guinea Pig! 2 + 3 = 5
```

A few rules of v0.1 syntax that the older pages get wrong:

- Output functions must be imported before use, for example `use std::poutln;`.
- `poo` declares an immutable variable and `mut` declares a mutable one.
  `<:` infers the type (`poo x <: 5;`); `=` follows an explicit type
  (`poo x int = 5;`). Reassignment uses `=` (`x = 1;`).
- Functions are declared with `poof`, parameters need types, and `>>` gives the
  return type: `poof add(a int, b int) >> int { ... }`.
- Comments are paired: `// like this //`.
- Logical operators are `and`, `or`, and `not` (`!` also works); `&&` and `||`
  do not exist.

## Where PooLang is going

PooLang is being repositioned as a sandboxed scripting language for AI agents:
an LLM agent writes the script and a host program runs it safely. The goals for
that design are explicit capabilities granted by the host, resource budgets,
deterministic execution, an interpreter that reports errors instead of
panicking, and diagnostics that an LLM can read and act on.

Version 0.2 will move to a more conventional syntax while keeping the `poo` and
`poof` keywords. Because of that change, this book will be rewritten against
v0.2 rather than patched page by page.
