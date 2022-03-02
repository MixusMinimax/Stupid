const A = 420;
const B = 69;

proc main 0

proc foo(a: int, b: int) -> int {
    let y: int;
    let x = {
        let c: int = a + b;
        c - 1
    };
    y = {
        let d = a * b;
        d * x
    };
    x + y
}