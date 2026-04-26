# Loops

PooLang provides two primary ways to repeat logic: `while` loops and `for` loops.

## While Loops

The `while` loop continues to execute as long as its condition remains `true`.

```poo
poo mut count << 0;

while count < 5 {
    pout("Count: ", count);
    count << count + 1;
}
```

## For Loops

PooLang supports `for` loops that iterate over a range or a collection.

### Range Iteration

You can iterate over a range of numbers using the `..` syntax:

```poo
for i in 0..5 {
    pout(i); # Prints 0, 1, 2, 3, 4
}
```

### Vector Iteration

You can also iterate over elements in a vector:

```poo
poo items << [10, 20, 30];

for item in items {
    pout(item);
}
```

## Scoping

The loop variable (like `i` or `item`) is only accessible within the body of the loop.
