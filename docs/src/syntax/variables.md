# Variables & Types

## Variable Declarations

PooLang uses the `poo` keyword to declare variables. By default, variables are **immutable**, meaning their value cannot be changed after assignment.

```poo
poo age << 5;
```

If you need to reassign a value, you must use the `mut` keyword:

```poo
poo mut score << 0;
score << 10; # This is allowed
```

## Assignment Operator

Note that PooLang uses the `<<` operator for assignment instead of the traditional `=`. This helps distinguish PooLang code at a glance.

## Supported Types

1. **Integer**: Whole numbers (e.g., `42`, `-10`).
2. **Float**: Decimal numbers (e.g., `3.14`, `2.0`).
3. **String**: Text enclosed in double quotes (e.g., `"Hello"`).
4. **Boolean**: Truth values (`true` or `false`).
5. **Vector**: A collection of values (e.g., `[1, 2, 3]`).

## Type Inference

PooLang automatically infers the type of a variable based on the value assigned to it. You do not need to explicitly declare types for variables.
