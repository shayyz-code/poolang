# Conditionals

> **Legacy page.** This page predates the v0.2 redesign and may show syntax
> the v0.1.11 interpreter does not accept. It will be rewritten in
> [P7 Docs truth](https://github.com/shayyz-code/poolang/milestone/8).

PooLang uses `if`, `elif`, and `else` to control the flow of execution based on boolean conditions.

## If Statements

The simplest form is a single `if` block:

```poo
if x > 10 {
    pout("x is large");
}
```

## If-Else

You can provide an alternative path using `else`:

```poo
if x > 10 {
    pout("x is large");
} else {
    pout("x is small");
}
```

## Elif (Else If)

For multiple conditions, use `elif`:

```poo
if x > 100 {
    pout("Huge");
} elif x > 10 {
    pout("Large");
} else {
    pout("Small");
}
```

## Scoping

Variables declared inside a conditional block are scoped to that block and are not accessible outside.

```poo
if true {
    poo temp << 1;
}
# temp is no longer accessible here
```
