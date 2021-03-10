# Elk

Elk is a simple interpreted programming language. Its goal is to combine the best language features from its inspirations, Rust and Javascript, while still being quick and easy to develop in.

## Usage

```bash
cargo run --release -- examples/ranges.elk
```

## Examples

```js
// This use statement will import a file named "std.elk" from the current directory
use "std.elk";

function print_n_times(text, n) {
    // 0..n defines a range, from 0 to n (including 0, excluding n).
    // For loops accept anything that can be converted to an iterator.
    for i in 0..n {
        // println is defined in ./std.elk
        println(i + ": " + text);
    }
}

let x = "this is a variable!!";
x = "Hello, World!";
print_n_times(x, 5);
// Output:
// 0: Hello, World!
// 1: Hello, World!
// 2: Hello, World!
// 3: Hello, World!
// 4: Hello, World!
```

```js
use "std.elk";

function fib_recursive(n) {
    // In Elk, conditionals do not require parenthesis, and using them is discouraged.
    if n <= 1 {
        return n;
    }
    return fib_recursive(n - 1) + fib_recursive(n - 2);
}
function fib_loop(n) {
    let n1 = 0;
    let n2 = 1;
    for i in 0..(n - 1) {
        let tmp = n1 + n2;
        n1 = n2;
        n2 = tmp;
    }
    return n2;
}
let num = 10;
println(fib_recursive(num)); // Returns 55
println(fib_loop(num)); // Returns 55
```