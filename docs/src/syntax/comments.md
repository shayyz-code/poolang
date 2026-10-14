# Comments

In PooLang v0.1.11 a comment opens with `//` and closes with another `//`.
Everything between the two markers is ignored, so a comment can sit between
statements on one line or span several lines.

```poo
use std::poutln;

// A comment opens and closes with two slashes. //
poutln("before"); // A comment can sit between statements. // poutln("after");
// It can also
   span several lines. //
```

This program prints `before` and then `after`.
