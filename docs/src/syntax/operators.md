# Operators & Math

PooLang supports standard arithmetic operations with proper mathematical precedence (PEMDAS).

## Arithmetic Operators

| Operator | Description | Example |
| :--- | :--- | :--- |
| `+` | Addition | `5 + 2` |
| `-` | Subtraction | `10 - 4` |
| `*` | Multiplication | `3 * 3` |
| `/` | Division | `20 / 5` |

### Precedence Example

```poo
poo result << 10 + 5 * 2; # result is 20 (5*2=10, 10+10=20)
```

## Comparison Operators

These operators return a **Boolean** (`true` or `false`).

| Operator | Description |
| :--- | :--- |
| `==` | Equal to |
| `!=` | Not equal to |
| `>` | Greater than |
| `<` | Less than |
| `>=` | Greater than or equal to |
| `<=` | Less than or equal to |

## Logical Operators

| Operator | Description |
| :--- | :--- |
| `&&` | Logical AND |
| `||` | Logical OR |
| `!` | Logical NOT |
