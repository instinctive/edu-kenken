# KenKen solver

For a puzzle of size `n`, the input will be `n` lines of `n` alphanumeric
characters. Each digit comprises a "given" cell value. Each non-digit comprises
one cell of an equation block. All cells sharing the same non-digit make up
that block.

For each non-digit, there must follow a line defining the equation. This
consists of the defining non-digit character, a space, the operator for the
equation, a space, and the value of the equation.

The operator must be one of `+`, '*', '-', and '/'.

## Example

    > ./kenken-exe
    AA
    A1
    A + 5
    12
    21
