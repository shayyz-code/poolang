# Standard Library

> **Legacy page.** This page predates the v0.2 redesign and may show syntax
> the v0.1.11 interpreter does not accept. It will be rewritten in
> [P7 Docs truth](https://github.com/shayyz-code/poolang/milestone/8).

PooLang includes a small built-in standard library to handle common tasks like output.

## Printing to Console

The `pout` function is used to print values to the standard output.

```poo
use std::pout;

pout("Hello, World");
pout("The result is: ", 42);
```

### Import Syntax

Note the `use std::pout;` line. `pout` is not available by default: calling it without importing it first fails with the runtime error `Undefined function: pout`.

## Future Plans

The standard library is currently minimal. Planned additions include:
- `std::math`: Advanced math functions (sin, cos, etc.)
- `std::fs`: File system operations
- `std::str`: String manipulation utilities
