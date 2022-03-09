const A = 420;
const B = 69 + A;

proc main(a := 1, b := 2.0) {
    let x = a;
    let y = x - B;
    let z = {
        let asd = y;
        y + 1
    };
    A + z
}
