# Standard Library

PooLang includes a small built-in standard library to handle common tasks like output.

## Printing to Console

The `pout` function is used to print values to the standard output.

```poo
use std::pout;

pout("Hello, World");
pout("The result is: ", 42);
```

### Import Syntax

Note the `use std::pout;` line. While `pout` is often available by default in many contexts, using the explicit `use` statement ensures it is imported into your current namespace.

## Future Plans

The standard library is currently minimal. Planned additions include:
- `std::math`: Advanced math functions (sin, cos, etc.)
- `std::fs`: File system operations
- `std::str`: String manipulation utilities
