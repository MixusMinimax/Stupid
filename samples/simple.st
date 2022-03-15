const A: int = 123 + 321;
const B = 0.4f * 2;

proc main {
    let x = foo(2);
    let y = bar(3, 2);
    x = x * y;
    x + 1
}

// This will not terminate, but that doesn't matter for syntax highlighting.
proc foo(a: int) bar(a)

proc bar(a: int, b := 10) {
    // annotating x as float resolves the type of bar, and therefore foo and main.
    let x: float = {
        let temp = foo(a - 1);
        temp - 1
    };
    x * b
}

proc min(a: int, b: int) -> int {
    if a < b a else b
}

// Automatic return type
proc max(a: int, b: int) {
    if a > b a else b
}