# PooLang v0.2 Language Reference (draft)

> [!WARNING]
> **This is the v0.2 target specification, not current behavior.** It is the
> contract that the P3 (refactor) and P4 (fixes) work implements. The v0.1.11
> interpreter does **not** accept most of the syntax below. For what runs today,
> see the rest of this book and the [changes from v0.1](#13-changes-from-v01).
> Choices that the owner has not yet confirmed are listed in
> [Open questions](#15-open-questions--decisions-made-in-this-draft).

PooLang is a sandboxed scripting language for AI agents. An LLM writes the
script and a host program runs it. The host grants capabilities and tools, sets
budgets, and gets either a result or a diagnostic the model can act on.
Execution is deterministic and the interpreter never panics on any input.

**Design rule.** Use conventional syntax that LLMs already write correctly, and
keep one canonical form per construct. Mistakes that models predictably make
(`let`, `elif`, `and`, `x++`, …) are not accepted as aliases. They produce a
diagnostic with a fix hint instead.

The words **must**, **must not**, **error** and **may** are normative. "Error
E0Nxx" means a diagnostic in that [code range](#11-diagnostics); exact
sub-codes are assigned during implementation.

## 1. Lexical structure

### 1.1 Source text

- Source is UTF-8. A leading byte-order mark is ignored. A file that is not
  valid UTF-8 is rejected with E08xx before lexing.
- Line terminators are `\n` and `\r\n`. Whitespace is space, tab, CR and LF.
  Any other character outside a string or comment that is not part of a token
  (including other Unicode whitespace) is error E01xx.
- Line and column numbers are 1-based. Columns count Unicode scalar values.

### 1.2 Comments

```text
// line comment, runs to the end of the line
/* block comment,
   may span lines */
```

Block comments do not nest: the first `*/` closes the comment. An unterminated
block comment is E01xx. Comments are allowed anywhere whitespace is.

### 1.3 Identifiers

```ebnf
identifier = ( letter | "_" ) { letter | digit | "_" } ;
letter     = "A" … "Z" | "a" … "z" ;
digit      = "0" … "9" ;
```

Identifiers are ASCII only and case-sensitive. A non-ASCII letter in an
identifier is E01xx with a hint to use ASCII. `_` alone is a valid identifier.
By convention, bindings and functions use `snake_case` and structs use
`PascalCase`; this is not enforced.

### 1.4 Keywords

These words are reserved and must not be used as identifiers:

```text
poo  poof  mut  if  else  while  for  in  break  continue  return
use  struct  impl  self  Self  true  false  null
```

`pub` is **not** a keyword in v0.2. Scripts have no visibility modifiers, so
every field and method is accessible. `pub` is a slip word (below).

### 1.5 Reserved slip words

These words are not part of the language. They are reserved so that the
predictable mistake gets a precise diagnostic (E02xx) with a fix hint, instead
of a confusing "unknown name" error later.

| Word | Hint |
|---|---|
| `let`, `var`, `const` | write `poo x = ...;` (add `mut` to reassign: `poo mut x = ...;`) |
| `fn`, `def`, `func`, `function` | declare functions with `poof name(a: T) -> R { ... }` |
| `elif` | write `else if` |
| `and` | use `&&` |
| `or` | use `\|\|` |
| `not` | use `!` |
| `None`, `nil`, `undefined` | use `null` |
| `True`, `False` | use `true` / `false` |
| `class` | use `struct` plus an `impl` block |
| `import`, `from` | write `use std::module::{a, b};` |
| `match`, `switch` | use `if` / `else if` chains |
| `loop` | write `while true { ... }` |
| `try`, `catch`, `throw`, `raise` | v0.2 has no exceptions; use `fail("message")` to abort |
| `pub` | v0.2 has no visibility; remove `pub` |
| `trait`, `inherits` | v0.2 has no traits or inheritance; compose structs instead |
| `step` | write `(a..b).step(n)` |
| `otherwise` | use `??` |

Slip words are only reserved as identifiers. They may appear as map keys
(`{"and": 1}`), inside strings and comments, and as member names after `.`.

### 1.6 Reserved type names

`int`, `float`, `bool`, `str`, `vec`, `map`, `any` and `range` name types.
They are not keywords, but they must not be declared as a binding, parameter,
function or struct name (E03xx, hint "`map` names a type; pick another name").
They may be used as field names, method names and map keys.

In type position, the slip type names `string`, `char`, `void`, `i64`, `f64`,
`list`, `dict` and `array` produce E02xx with hints (`str`, `str`, omit the
return type, `int`, `float`, `vec[T]`, `map[str, V]`, `vec[T]`).

### 1.7 Integer literals

```ebnf
int_lit       = "0" | nonzero_digit { [ "_" ] digit } ;
nonzero_digit = "1" … "9" ;
```

- Decimal only. No hex, octal or binary literals in v0.2.
- `_` separators are allowed between digits: `1_000_000`. A separator must not
  be leading, trailing or doubled.
- Leading zeros are an error (`007`, E01xx) to avoid octal confusion.
- Integer literals have type `int` (i64). A literal larger than
  `9223372036854775807` is E01xx. Because `-` is a unary operator,
  `i64::MIN` cannot be written as a literal; use `std::math::INT_MIN`.

### 1.8 Float literals

```ebnf
float_lit = int_lit "." digits [ exponent ]
          | int_lit exponent ;
exponent  = ( "e" | "E" ) [ "+" | "-" ] digits ;
digits    = digit { [ "_" ] digit } ;
```

- A `.` must have a digit on both sides: `1.0` and `0.5` are valid; `1.` and
  `.5` are E01xx with a hint (`1.0`, `0.5`).
- `1e3`, `2.5E-4` and `1_000.5` are floats.
- `1..5` lexes as `1`, `..`, `5` (a range), not as a float.
- Float literals have type `float` (IEEE 754 binary64).

### 1.9 String literals

Strings use double quotes `"..."` or single quotes `'...'`. Both produce a
`str`; there is no `char` type. A string may span lines; the line break is part
of the value.

| Escape | Meaning |
|---|---|
| `\n` `\t` `\r` | newline, tab, carriage return |
| `\\` `\"` `\'` `\/` | literal `\`, `"`, `'`, `/` |
| `\0` `\b` `\f` | NUL, backspace, form feed |
| `\u{X…}` | Unicode scalar, 1–6 hex digits |
| `\uXXXX` | JSON form, exactly 4 hex digits; a surrogate pair written as two `\uXXXX` escapes is combined |

Any other escape, a lone surrogate, or a code point above `U+10FFFF` is E01xx.
An unterminated string is E01xx and the span points at the opening quote.

### 1.10 Punctuation and operators

```text
(  )  [  ]  {  }  ,  ;  :  ::  .  ..  ->  ?
=  +=  -=  *=  /=  %=
+  -  *  /  %  !  &&  ||  ??
==  !=  <  <=  >  >=
```

Tokens are formed by longest match. `++`, `--`, `**`, `===`, `!==`, `=>`,
`<:` and `>>` are not tokens; each produces E02xx with a hint (`x += 1`,
`x -= 1`, `std::math::pow`, `==`, `!=`, `poof (...) { ... }`, `=`, `->`).

## 2. Types

| Type | Values | Notes |
|---|---|---|
| `int` | 64-bit signed integers | Overflow is a runtime error (E04xx), never wraparound. |
| `float` | IEEE 754 binary64 | Follows IEEE semantics, including `inf` and `NaN`. |
| `bool` | `true`, `false` | |
| `str` | immutable Unicode text | `.len()` counts Unicode scalar values (chars), not bytes. |
| `null` | `null` | The only value of type `null`. |
| `vec[T]` | ordered, growable list | Reference type. |
| `map[str, V]` | insertion-ordered string-keyed map | Reference type. Keys must be `str` in v0.2. |
| `range` | integer range from `a..b` | Iterable; see [§4.4](#44-ranges). |
| struct `Name` | user-defined record | Reference type. |
| `poof(T, U) -> R` | function or closure value | |
| `any` | any value, including `null` | Disables the runtime check for that annotation. |
| `T?` | `T` or `null` | Plain `T` never accepts `null`. |

- **No implicit conversions.** `1 + 1.0` is E04xx with the hint
  "convert explicitly: `x.to_float()`". Conversions are explicit methods:
  `int.to_float()`, `float.to_int()` (truncates toward zero; E04xx if the value
  is non-finite or out of range), `x.to_str()`, `str.to_int()` and
  `str.to_float()` (E04xx if the text does not parse).
- `vec` and `map` must always be parameterized in annotations: `vec[any]`, not
  `vec`.
- `map[int, V]` (any key type other than `str`) is E03xx.
- **Reference semantics.** Assigning or passing a `vec`, `map` or struct shares
  the same object. `.clone()` makes a deep copy. `str`, `int`, `float`, `bool`
  and `null` are immutable values.

## 3. Bindings

```text
poo name = expr;            // immutable binding
poo mut count = 0;          // mutable binding
poo limit: int = 10;        // annotated
poo mut best: float? = null;
```

- Every binding must be initialized.
- An annotation is checked at runtime when the binding is initialized and on
  every later assignment. A mismatch is E04xx naming the expected and actual
  types.
- An unannotated binding accepts any value on reassignment. Type checks apply
  only where there is an annotation.
- Reassignment `name = expr;` is allowed only if `name` was declared `mut`.
  Otherwise E03xx with the hint "declare it as `poo mut name = ...`".
  Assigning to an undeclared name is E03xx with the hint "declare it with
  `poo name = ...`".
- **Compound assignment** `+= -= *= /= %=` is shorthand for
  `x = x op y` and follows the same rules. Assignment is a statement, not an
  expression; `poo a = b = 1;` is E02xx.
- **Mutation through a binding.** Assigning to a field or index
  (`p.x = 1;`, `v[0] = 1;`, `m["k"] = 1;`) and calling a mutating method
  (`v.push(x)`, `m.remove(k)`, …) require the root binding of the path to be
  `mut` (E03xx otherwise). Because values are shared by reference, this is a
  rule about the binding, not deep immutability: another `mut` alias can still
  change the object.

### 3.1 Scope

- Scoping is lexical. Every `{ ... }` block opens a new scope; function bodies
  and loop bodies are blocks.
- Declaring a name that already exists **in the same scope** is E03xx.
  Parameters are in the same scope as the top level of the function body.
- Declaring a name that exists in an **enclosing** scope shadows it until the
  end of the inner block.
- Reading a name before its `poo` statement has run is E04xx ("used before
  initialization").
- `poof` and `struct` declarations are hoisted to the top of their block, so
  functions may call each other regardless of order.
- Closures capture enclosing bindings by reference: later assignments to a
  captured `mut` binding are visible inside the closure and vice versa.

## 4. Expressions

### 4.1 Operator precedence

From lowest to highest. All binary operators are left-associative except
where noted.

| Level | Operators | Operand types | Notes |
|---|---|---|---|
| 1 | `??` | any | `a ?? b` is `a` unless `a` is `null`; `b` is evaluated only if needed. |
| 2 | `\|\|` | `bool` | Short-circuits. |
| 3 | `&&` | `bool` | Short-circuits. |
| 4 | `==` `!=` | see [§4.3](#43-equality) | Non-associative. |
| 5 | `<` `<=` `>` `>=` | `int`/`int`, `float`/`float`, `str`/`str` | Non-associative. Strings compare by code point. |
| 6 | `..` | `int`/`int` | Non-associative. Produces a `range`. |
| 7 | `+` `-` | `int`, `float`; `+` also on `str`/`str` | `+` on two `str` concatenates. |
| 8 | `*` `/` `%` | `int`, `float` | See [§4.2](#42-arithmetic). |
| 9 | unary `!` `-` | `!` on `bool`; `-` on `int`, `float` | Prefix, right-associative. |
| 10 | call `f(...)`, index `a[i]`, member `a.b`, path `A::b` | | Postfix, left to right. |

- Chaining a non-associative operator (`a < b < c`, `a == b == c`,
  `a..b..c`) is E02xx with the hint "split it: `a < b && b < c`".
- `&&` and `||` require `bool` operands (E04xx otherwise). There is no
  truthiness.
- Parentheses group as usual.
- There is no ternary `? :`. It is E02xx with the hint "use `if`/`else`".

### 4.2 Arithmetic

- Both operands must have the same numeric type (E04xx otherwise).
- `int` `/` truncates toward zero. `int` `%` takes the sign of the dividend.
  Integer division or remainder by zero is E04xx.
- Any `int` result outside the i64 range (including `-INT_MIN` and
  `INT_MIN / -1`) is E04xx.
- `float` arithmetic follows IEEE 754: `1.0 / 0.0` is `inf`, `0.0 / 0.0` is
  `NaN`.
- `str + str` concatenates. `str + int` is E04xx with the hint
  "convert explicitly: `n.to_str()`".

### 4.3 Equality

- `==` and `!=` compare values of the same type: numbers and booleans by value,
  strings by content, `vec` element by element, `map` by key set and values
  (key order does not matter), structs by type and field values.
- Any value may be compared with `null`. `x == null` is `true` only when `x`
  is `null`.
- Comparing two non-null values of different types (`1 == 1.0`, `"1" == 1`)
  is E04xx with a conversion hint. This catches a common LLM bug instead of
  silently returning `false`.
- Comparing function values is E04xx.
- `NaN == NaN` is `false`, as in IEEE 754.
- Comparing structures that contain a cycle is E04xx.

### 4.4 Ranges

- `a..b` is an expression of type `range` over the integers
  `a, a+1, …, b-1`. It is half-open; `..=` is E02xx with the hint
  "write `a..(b + 1)`".
- If `a >= b` the range is empty.
- Ranges are iterable and have methods: `.step(n)` (`n > 0`, else E04xx),
  `.rev()`, `.len()`, `.contains(i)` and `.to_vec()`.
- Ranges cannot index or slice: `v[1..3]` is E04xx with the hint
  "use `v.slice(1, 3)`".

### 4.5 Indexing and member access

- `v[i]` on a `vec` requires an `int` index with `0 <= i < v.len()`. Anything
  else is E04xx. Negative indices are not supported. `v.get(i)` returns `null`
  when out of range.
- `m[k]` on a `map` requires a `str` key that exists (E04xx otherwise, naming
  the missing key). `m.get(k)` returns `null` for a missing key, so the
  idiomatic default is `m.get(k) ?? fallback`.
- Strings cannot be indexed; use `s.chars()[i]` (E04xx hint).
- `a.b` is field access on a struct. `m.name` on a map is E04xx with the hint
  "maps use `m[\"name\"]`".
- `a.b(...)` always calls the method `b`. To call a function stored in a
  field, write `(a.b)(...)`.
- `A::b` refers to an item `b` of a module or an associated function of a
  struct.

## 5. Literals

```text
42   3.14   1e-3   "text"   'text'   true   false   null
[1, 2, 3]                          // vec
[]                                 // empty vec
{"name": "Ada", "age": 36}         // map
{name: "Ada", age: 36}             // same map: bare keys are str keys
{}                                 // empty map (in expression position)
Point { x: 1, y: 2 }               // struct literal
```

- Trailing commas are allowed in every comma-separated list: literals,
  parameters, arguments and `use` lists.
- Map keys are either string literals or bare identifiers. A bare identifier
  key is the string of its name; it is never evaluated as a variable. Keys may
  be any identifier, including keywords and slip words (`{if: 1}`).
- A duplicate key in one map literal is E02xx.
- Because of the above, every JSON document is a valid PooLang expression, with
  one exception: JSON objects with duplicate keys are rejected.
- `{` at the start of a statement always opens a block, never a map.
- A struct literal must list every field exactly once (E04xx for a missing,
  unknown or duplicate field). Field values are checked against the declared
  field types.
- A struct literal is not allowed directly as the condition of `if`/`while` or
  the iterable of `for`; wrap it in parentheses. `if done { ... }` therefore
  never parses as a struct literal.

## 6. Statements and control flow

A script is a sequence of statements executed top to bottom.

- **Semicolons are required** after bindings, assignments, expression
  statements, `return`, `break`, `continue` and `use`. They are not written
  after block statements (`if`, `while`, `for`, `poof`, `struct`, `impl`). A
  missing semicolon is E02xx with the hint "add `;`" pointing just after the
  statement.
- **Blocks** `{ ... }` introduce a scope.

```text
if score >= 90 {
    println("A");
} else if score >= 75 {
    println("B");
} else {
    println("C");
}

poo mut i = 0;
while i < 10 {
    i += 1;
    if i % 2 == 0 { continue; }
    if i > 7 { break; }
}

for n in 0..5 { println(n); }
for n in (0..10).step(2) { println(n); }
for item in items { println(item); }
for key in config { println(key, config[key]); }
for entry in config.entries() { println(entry[0], entry[1]); }
```

- `if` and `while` conditions must be `bool` (E04xx with the hint "compare
  explicitly, e.g. `x != null` or `v.len() > 0`"). Braces are required; there
  is no single-statement form. `if` is a statement, not an expression.
- `for x in e` accepts a `vec` (elements), a `range` (integers) or a `map`
  (keys in insertion order). Anything else is E04xx. Use `m.values()` or
  `m.entries()` for values or `[key, value]` pairs. There is no destructuring
  in v0.2.
- The loop variable is a fresh immutable binding on each iteration.
- `for` iterates over a shallow snapshot of the collection taken when the loop
  starts. Changing the collection inside the loop does not affect the
  iteration.
- `break` and `continue` apply to the innermost loop. Outside a loop they are
  E02xx. There are no labels.
- `return expr;` and `return;` leave the current function. At the top level,
  `return` ends the script and the value becomes the **script result** handed
  to the host (`null` if omitted or if the script runs to the end).

## 7. Functions

```text
poof add(a: int, b: int) -> int {
    return a + b;
}

poof greet(name: str) {             // no return type: returns null
    println("hello", name);
}

poof push_twice(mut items: vec[int], x: int) {
    items.push(x);
    items.push(x);
}

poo double = poof (x: int) -> int { return x * 2; };
```

- Parameter types are **required**. The return type is optional; without it
  the function returns `null`.
- A `mut` parameter is a mutable binding. Without `mut`, the parameter cannot
  be reassigned or mutated through.
- Arguments are checked against parameter types at call time (E04xx). The
  wrong number of arguments is E04xx (or E03xx when it is known statically).
  There are no default, named or variadic parameters in user functions.
- The return value is checked against the declared return type. Reaching the
  end of a function returns `null`; if the declared return type does not accept
  `null`, that is E04xx ("missing return").
- Recursion is allowed. Depth is limited by the call-depth budget
  ([§12](#12-budgets)).
- Functions are first-class values with type `poof(T, U) -> R`. An anonymous
  function is written `poof (params) -> R { ... }` in expression position.
  Named `poof` declarations nested in a block are also closures.

### 7.1 Prelude

These functions are always in scope without `use`:

| Function | Effect |
|---|---|
| `print(a, b, …)` | Writes the display form of each argument, separated by one space, to the host's output sink. |
| `println(a, b, …)` | Same as `print`, then a newline. |
| `type_of(x)` | Returns the type name as `str`: `"int"`, `"vec"`, `"Point"`, … |
| `fail(message)` | Aborts the script with runtime error E04xx carrying `message`. |

**Display form** is deterministic. Strings print raw at the top level and
quoted inside collections. Floats always show a fractional part (`1.0`) and use
the shortest representation that round-trips. `vec` prints as `[1, 2]`, `map`
as `{"a": 1}` in insertion order, structs as `Point { x: 1, y: 2 }`, and a
reference back into an enclosing value as `<cycle>`.

## 8. Structs and methods

```text
struct Point {
    x: float,
    y: float,
}

impl Point {
    poof new(x: float, y: float) -> Self {
        return Point { x: x, y: y };
    }

    poof len(self) -> float {
        return std::math::sqrt(self.x * self.x + self.y * self.y);
    }

    poof scale(mut self, k: float) {
        self.x *= k;
        self.y *= k;
    }
}

poo mut p = Point::new(3.0, 4.0);
println(p.len());   // 5.0
p.scale(2.0);
```

- A struct declares named, typed fields separated by commas. Field types are
  checked on construction and on every field assignment.
- `impl Name { ... }` declares methods and associated functions for a struct
  declared in the same script. Several `impl` blocks per struct are allowed; a
  duplicate method name is E03xx.
- A method's first parameter is `self` or `mut self`, with no type. Methods
  that change fields must take `mut self`, and calling one requires the
  receiver's root binding to be `mut`.
- A function in `impl` without `self` is an associated function, called as
  `Name::f(...)`.
- `Self` inside an `impl` block names the struct type.
- Struct values are references ([§2](#2-types)).
- There is no inheritance and no traits. Share behavior by composing structs
  and writing functions.

## 9. Modules and capabilities

```text
use std::math::{sqrt, PI};
use std::json;
use tools::search;
```

- `use` statements are allowed only at the top level of a script, before any
  other statement (E02xx otherwise).
- `use a::b::c;` brings `c` into scope. `use a::b::{c, d};` brings several
  items. `use std::json;` brings the module, so its items are written
  `json::parse(...)`. Fully qualified paths such as `std::math::sqrt(x)` work
  without `use`. There is no `*` import and no renaming.
- Items are resolved **before execution starts**. An unknown module or item is
  E03xx.
- **Pure `std` modules are always available**: `std::math` (functions and
  constants such as `sqrt`, `pow`, `abs`, `min`, `max`, `floor`, `PI`,
  `INT_MAX`, `INT_MIN`) and `std::json` (`parse`, `stringify`). The built-in
  methods on `str`, `vec`, `map` and `range` need no import.
- **Effectful modules need a host grant**: `std::time` (`now`) needs the
  `time` capability, and `std::rand` (seeded by the host) needs the `rand`
  capability.
- **Host tools** live under `tools::`. Every `tools::*` function needs an
  explicit host grant. Tool arguments and results are JSON values (`null`,
  `bool`, `int`, `float`, `str`, `vec`, `map`).
- Using an ungranted capability or tool is E06xx and is reported before any
  statement runs. The hint names the capability, e.g.
  "grant `tools::search` to the engine (CLI: `--allow tools::search`)".
- There is no filesystem, network, process or environment access except through
  host tools. Output goes only to the host's output sink via `print` and
  `println`.

## 10. Null and errors

- `null` is the absence of a value. Only `T?`, `null` and `any` annotations
  accept it.
- `a ?? b` gives a default for `null`. Calling a method, accessing a field or
  indexing `null` is E04xx with a hint to use `??` or a `!= null` check.
- A runtime error aborts the script and produces one diagnostic. Output written
  before the error is kept.
- There is no `try`/`catch` in v0.2. Recoverable error handling is planned for
  a later version.

## 11. Diagnostics

Every failure is reported as a diagnostic:

```text
{
  "schema": "poo.diagnostic/v1",
  "code": "E0203",
  "kind": "parse",
  "message": "expected `;` after statement",
  "span": { "line": 4, "col": 18, "end_line": 4, "end_col": 18 },
  "hint": "add `;`"
}
```

- `code` is `E` followed by four digits. The first two digits give the range:

| Range | `kind` | Raised by |
|---|---|---|
| E01xx | `lex` | Lexer: bad characters, literals, escapes, unterminated strings or comments. |
| E02xx | `parse` | Parser: unexpected tokens, slip words, missing `;`, nesting limit. |
| E03xx | `check` | Static checks before execution: undeclared names, redeclaration, assignment to non-`mut`, unknown modules. |
| E04xx | `runtime` | Execution: type mismatch, overflow, out-of-bounds, missing key, `fail(...)`. |
| E05xx | `budget` | Exceeded steps, call depth, memory or deadline, or host interrupt. |
| E06xx | `capability` | Ungranted capability or tool. |
| E07xx | `host` | A host tool returned an error or an invalid value. |
| E08xx | `io` | CLI and I/O: unreadable file, invalid UTF-8, bad flags. |

- `span` uses 1-based lines and columns (columns in chars) and is always
  present for E01–E06. It may be absent for E07 and E08.
- `hint` is a single actionable sentence, or `null`.
- The message and hint are written for an LLM reader: they say what was found,
  what was expected, and how to fix it, and they quote the user's code.
- The full schema, the sub-code catalogue and the human-readable rendering are
  specified in P5.

## 12. Budgets

The host sets limits for each run. Exceeding one aborts the script with E05xx.
Defaults and exact accounting are specified in P5.

| Budget | Limits |
|---|---|
| Steps | Number of evaluated statements and expressions. |
| Call depth | Nesting of function calls, including recursion. |
| Memory | Approximate bytes held by live values. |
| Deadline | Wall-clock time. This is the only budget whose outcome can vary between runs. |

The host can also interrupt a running script, which is reported as E05xx.

## 13. Changes from v0.1

There is no compatibility mode. v0.1 scripts must be rewritten.

| v0.1 | v0.2 |
|---|---|
| `poo x <: 1;` | `poo x = 1;` |
| `poo x int = 1;` | `poo x: int = 1;` |
| `mut x <: 0;` / `mut d bool = true;` | `poo mut x = 0;` / `poo mut d: bool = true;` |
| `poof f(a int) >> int { ... }` | `poof f(a: int) -> int { ... }` |
| `>> void` | omit the return type |
| `vec:int` | `vec[int]` |
| `map` | `map[str, V]` |
| `// comment //` | `// comment` or `/* comment */` |
| `and` / `or` / `not` | `&&` / `\|\|` / `!` |
| `elif` | `else if` |
| `x otherwise y` | `x ?? y` |
| `pout(...)` / `poutln(...)` | `print(...)` / `println(...)` (prelude, no `use`) |
| `use std::(a, b);` | `use std::{a, b};` (`print`/`println` need no `use`) |
| `use math::PI;` | `use std::math::PI;` |
| `Name::{ a: 1 }` | `Name { a: 1 }` |
| fields `name str` (no separators) | `name: str,` |
| `impl { ... }` inside `struct` | separate `impl Name { ... }` block |
| `inherits`, `trait`, `impl Parent { ... }` | removed |
| `pub` fields | removed |
| `'c'` (`char`) | `'c'` is a one-character `str`; `char` type removed |
| `s.chars()` → `vec:char` | `s.chars()` → `vec[str]` |
| `s.len()` in bytes | `s.len()` in chars |
| `i._f()` / `f._i()` | `i.to_float()` / `f.to_int()` |
| `[x >> n]` | `[x].repeat(n)` |
| `for i in 0..10 step 2` | `for i in (0..10).step(2)` |
| `v.nth(i)` | `v.get(i)` or `v[i]` |
| `s.nth_char_otherwise_(i, c)` | `s.chars().get(i) ?? c` |
| `v.push(x)` / `v.pop()` return a new vec | they mutate `v` in place; `pop` returns the removed element or `null` |
| `cls()` | removed (the host owns the terminal) |
| `time::sleep(ms)` | removed; `std::time` is capability-gated |
| no `<=`, `>=`, `%`, `break`, `continue`, `null` | all supported |

## 14. Example

An agent script that asks a host tool for order records, filters them, and
aggregates the result:

```text
use tools::fetch_orders;

struct Total {
    count: int,
    amount: float,
}

impl Total {
    poof add(mut self, amount: float) {
        self.count += 1;
        self.amount += amount;
    }
}

poof is_billable(order: map[str, any]) -> bool {
    return order["status"] == "paid" && !(order.get("refunded") ?? false);
}

poo orders: vec[map[str, any]] = fetch_orders({region: "eu", limit: 100});
poo mut by_customer: map[str, Total] = {};

for order in orders {
    if !is_billable(order) {
        continue;
    }
    poo customer: str = order["customer"];
    if !by_customer.has(customer) {
        by_customer[customer] = Total { count: 0, amount: 0.0 };
    }
    by_customer[customer].add(order["amount"]);
}

poo mut report = [];
for customer in by_customer {
    poo t = by_customer[customer];
    println(customer, t.count, t.amount);
    report.push({customer: customer, orders: t.count, total: t.amount});
}

return report;
```

`by_customer[customer].add(...)` is allowed because the root binding
`by_customer` is `mut`. If the host has not granted `tools::fetch_orders`, the
script stops before the first statement with an E06xx diagnostic naming that
tool.

## 15. Open questions / decisions made in this draft

The approved plan fixed the syntax direction (conventional syntax with `poo`
and `poof`; `=`, `name: type`, `->`, `&&`/`||`/`!`, `<=`/`>=`, `%`, `//` and
`/* */`, `null`, `??`, `else if`, `break`/`continue`, JSON-style literals,
`print`/`println`, both quote styles with no `char`, reserved slip words, no
compatibility mode, no `inherits`/`trait`). Everything below was decided while
writing this draft and needs the owner's review.

**Lexical**

1. `pub` is dropped (no visibility in scripts) and becomes a slip word.
2. Slip words added beyond the core set (`let fn def var const function elif
   and or not`): `func`, `None`, `nil`, `undefined`, `True`, `False`, `class`,
   `import`, `from`, `match`, `switch`, `loop`, `try`, `catch`, `throw`,
   `raise`, `pub`, `trait`, `inherits`, `step`, `otherwise`.
3. Type names (`int float bool str vec map any range`) are contextual: they
   cannot be declared as names but may be field, method or key names. The slip
   type names `string char void i64 f64 list dict array` get hints.
4. Identifiers are ASCII only.
5. `_` digit separators are allowed (`1_000`); leading zeros are rejected; no
   hex, octal or binary literals.
6. Floats need a digit on both sides of `.`; exponents `e`/`E` with an optional
   sign are accepted (needed for JSON).
7. JSON escapes `\uXXXX`, `\/`, `\b` and `\f` are accepted alongside
   `\u{X…}`, and `\0` is added, so that JSON strings parse unchanged.
8. Strings may contain literal line breaks.
9. Block comments do not nest.
10. `++ -- ** === !== => <: >>` and `..=` are rejected with hints rather than
    accepted.

**Types and values**

11. New types: `any` (needed for JSON data from tools) and `range`.
12. Nullable types are written `T?`; plain `T` rejects `null`.
13. Function types are written `poof(T, U) -> R`.
14. `float` follows IEEE 754 (`inf`, `NaN` allowed); `float.to_int()` errors on
    non-finite or out-of-range values. Integer overflow and division by zero
    are errors.
15. `int` division truncates toward zero; `%` takes the sign of the dividend.
16. `vec`, `map` and struct values are references; `.clone()` deep-copies.
17. `map` equality ignores key order; insertion order is kept for iteration
    and display.

**Bindings and mutation**

18. Unannotated bindings are dynamically typed on reassignment; annotated ones
    are checked on every assignment.
19. Field assignment, index assignment and mutating methods require the root
    binding to be `mut` (shallow, binding-level rule). Parameters can be
    declared `mut`, and mutating methods take `mut self`.
20. Compound assignment `+= -= *= /= %=` is included; `++`/`--` are not.
21. Shadowing is allowed in an inner block; redeclaring in the same scope
    (including parameters at function top level) is an error.
22. `poof` and `struct` declarations are hoisted within their block.

**Expressions**

23. Comparing two non-null values of different types with `==`/`!=` is a
    runtime error, not `false`.
24. `..` sits between comparison and `+`/`-` in precedence; ranges are
    half-open; stepping is `.step(n)` with `n > 0` and reversing is `.rev()`.
25. No slicing syntax: `v.slice(a, b)`. No negative indices. No string
    indexing: `s.chars()[i]`.
26. Map access is `m["k"]` only; `m.k` on a map is an error with a hint, which
    keeps one canonical form. `m.get(k)` and `v.get(i)` return `null`.
27. `a.b(...)` always calls a method; calling a function stored in a field
    needs `(a.b)(...)`.
28. Duplicate keys in a map literal are an error, so JSON with duplicate keys
    is rejected.
29. A struct literal is not allowed directly in an `if`/`while`/`for` head
    (Rust's rule).
30. No ternary operator and no `if` expressions.

**Statements and functions**

31. `for` over a map yields keys; `.entries()` yields `[key, value]` vecs; no
    destructuring.
32. `for` iterates over a snapshot taken when the loop starts.
33. Top-level `return expr;` ends the script and sets the script result for
    the host.
34. Anonymous functions are written `poof (x: int) -> int { ... }`; there is
    no arrow or `|x|` syntax.
35. No default, named or variadic parameters (the prelude `print`/`println`
    are the only variadic functions).
36. Several `impl` blocks per struct are allowed.

**Modules, output and host**

37. Prelude (no `use` needed): `print`, `println`, `type_of`, `fail`.
38. `print`/`println` separate arguments with one space (v0.1 used no
    separator). Display form is fixed as described in §7.1, including
    `<cycle>` for cyclic values.
39. `use` is allowed only at the top of a script; no glob imports or renaming;
    fully qualified paths work without `use`.
40. Capability and tool grants are checked before execution starts, not when
    the call happens.
41. `std::time::sleep` and `cls` are removed. `std::rand` is seeded by the
    host.
42. Tools take and return JSON values.

**Diagnostics**

43. Codes are `E` plus four digits; columns are counted in chars; JSON field
    names are `code`, `kind`, `message`, `span` (`line`, `col`, `end_line`,
    `end_col`) and `hint`, plus a `schema` field.
44. Slip words are parse errors (E02xx); misuse of type names as bindings is a
    check error (E03xx).

**Still open** (not settled by this draft): the default budget values, whether `std::rand` exists
at all in v0.2, string interpolation, a recoverable error mechanism, and
whether unannotated `mut` bindings should keep the type of their first value.
