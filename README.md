# Stupid

This is a compiler for a made up language, mostly to learn about compiler design, and formal languages.

However, I plan on using this language for actual programs, one of the goals is to make it self-hosted.

There is a language extension for VsCode/Sublime/Textmate: [gh:MixusMinimax/stupid-code](https://github.com/MixusMinimax/stupid-code)

Until there is proper documentation, just take a look at the [samples](./samples/) folder.

## Current State

### TODO
- [X] Lexing
- [X] Parsing
- [X] Textmate Grammar
- [X] Type analysis
- [ ] AST simplification (Almost done)
- [ ] AST function evaluation for constants
- [ ] Compilation to an intermediary language (something like BRIL)
- [ ] Emulator for that intermediary language
- [ ] Compilation to actual assembly
  - [ ] x_86
  - [ ] x_64
  - [ ] aarch64
- [ ] standard library
- [ ] decide on library system (C-like #include? Modules?)

### Examples of current type-analysis and simplification

Input code:

```stupid
const A: int = 123 + 321;
const B = 0.4f * 2;
const C = 123 == 321 || 2 == 2 && 1;

proc main 1 - add(1 + 2, -3)

proc add(a: int, b := -(2 - 3)) -> int {
    (a + b) * { 1 * { 5 - (4 / 2) } } / sub(1, { 1 + 2 }) + sub(3)
}

proc sub(a: int, b := 1) {
    a - b
}

proc ifelse {
    if 123 == 321 || 2 == 2 && 1 {
        1 + 2
    } else {
        3 + 4
    }
}

proc foo {
    bar()
}

proc bar {
    // This will resolve the type for both foo and bar.
    let a: long = foo();
    a
}
```

Stringified AST after analysis:

```stupid
const A: int = 444;
const B: float = 0.8f;
const C: bool = true;

proc main() -> int (1 - add(3, -3))

proc add(a: int, b: int := 1) -> int ((((a + b) * 3) / sub(1, 3)) + sub(3))

proc sub(a: int, b: int := 1) -> int (a - b)

proc ifelse() -> int 3

proc foo() -> long bar()

proc bar() -> long {
    let a: long = foo();
    a
}
```