const SYS_WRITE = 1;

// This is a comment.
proc main() -> int {
    let x: int = 123;
    let y: int = 321;
    let z: int = 456;
    let result: int = /* Inline comment. */ (x + y) * z;
    /*
     * This is a multiline comment.
     * This syscall will only work on linux, cross-platform
     * stuff will be implemented using conditional compilation
     * within the language itself.
     */
    syscall(SYS_WRITE, 1, "Hello, World!\n", 14);
    /*
     * This is another multiline comment.
     * The return value of this function will be used as the exit code.
     */
    return result;
}