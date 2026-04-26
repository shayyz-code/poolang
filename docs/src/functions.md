# Functions

Functions in PooLang allow you to encapsulate logic into reusable blocks. They are defined using the `poof` keyword.

## Defining a Function

A basic function looks like this:

```poo
poof sayHello() {
    pout("Hello!");
}
```

## Return Types

PooLang uses the `>>` operator to specify the return type of a function.

```poo
poof getNumber() >> int {
    return 42;
}
```

Common return types include `int`, `float`, `string`, `bool`, and `void` (if no value is returned).

## Parameters

You can pass data into functions via parameters:

```poo
poof add(a, b) >> int {
    return a + b;
}

poo sum << add(5, 5); # sum is 10
```

## Function Scope

Functions create their own scope. Variables declared inside a function are not accessible outside. Functions can, however, access variables in the global scope if they are defined before the function is called.
