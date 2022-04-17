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

proc inline_if {
    if true {
        foo();
        bar();
    };
    if false {
        add(1);
        sub(2);
    };
}