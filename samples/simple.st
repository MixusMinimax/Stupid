const A = 420;
const B = 69 + A;

proc main foo()

proc foo bar()

proc bar {
    // Annotating x as float resolves the types of foo, bar, and main.
    let x: float = foo() + B;
    let y = x = x + 1;
    y
}