# Work completed

I completed all the mandatory tasks in addition to the following extensions:

# Extensions

## Type-preserving erasure

The goal of this extension is to obtain, from a `μCoreJ` program, a `μCore` program that preserves both the typing and the semantics of the original `μCoreJ` program. To achieve this, all occurrences of `jump` in the `μCoreJ` program must be in tail position. In order to obtain a `μCoreJ` program in which all occurrences of `jump` are in tail position, the optimization rules `commute` and `abort` must be applied as much as possible. Once this is done, all `jumps` will be in tail position (as proven in the paper *Luke Maurer, Paul Downen, Zena M. Ariola, and Simon L. Peyton Jones (2017), Compiling without continuations*). This is convenient because the mandatory tasks require implementing these two rules, which are essential for the soundness of erasure. One can then replace all `joins` with function definitions and all `jumps` with calls to these functions.

To implement this, I ensure that all calls to the erasure procedure are performed on programs optimized with `case-of-case` (to satisfy the conditions required for the soundness of erasure). For the erasure itself, I use a function that removes all `joins` and `jumps` from a given typed term. Since the program must be optimized beforehand with `case-of-case`, we can simply replace all `joins` and `jumps` without further concern. One detail is that this function returns an untyped term, because we do not know what information to provide when constructing the function call that replaces a `jump`. Finally, I have a function that erases a program: it takes a typed program, erases its `joins` and `jumps`, then type-checks it (to obtain a typed program as output) and returns it.

To test the erasure, I optimize all the test files using `case-of-case`, then I compare the obtained result with the expected result.

To ensure that erasure is always applied to a program optimized with `case-of-case`, this is handled in the file `joujou.ml`. The implementation of the erasure itself is located in the file `eraser.ml`. The test files are located in the `test` directory.

## Mutually recursive let and join

The goal of this extension is to add mutually recursive `let` and `join` constructs to the supported language.

To implement these two new constructs, I added a recursive definition for `let rec` in the parser, which is essentially the non-recursive definition with a mandatory type annotation. For `join rec`, I reused the same parsing rules as for `join`, but with the additional `rec` keyword. To enable mutually recursive definitions for `let` and `join`, I require the keyword `and` to separate each definition.

I then extended the typechecker to support `let rec` and `join rec`. Typing is performed globally, as for `let` and `join`, but with the additional step of inserting `x` and `j` into the environment for both the definitions and the body. This is why a mandatory type annotation is required for `let rec`.

After that, I added `let rec` and `join rec` to the simplifier, along with the `drop` rule for `let rec` and the `jdrop` rules for `join` and `join rec`, as described in the paper *Luke Maurer, Paul Downen, Zena M. Ariola, and Simon L. Peyton Jones (2017), Compiling without continuations*.

Finally, I added support for these constructs in the erasure phase. `let rec` is erased as `let rec`, and `join rec` is erased as `let rec` by constructing each definition and the body in the same way as for `join`. The type annotation of the resulting `let rec` can be obtained from the explicit type of `join rec` and from the types of its arguments, which are inferred beforehand.

To test all of these features, I added new test files for each stage (typechecking, simplifying, and erasing).

Since this extension affects most stages of the compiler, I modified many files. The most interesting changes can be found in `eraser.ml`, `parser.mly`, `simplifier.ml`, `terms.ml`, and `typecheck.ml`. The less interesting changes are in `Makefile`, `internalize.ml`, `lexer.mll`, `print.ml`, `symbols.ml`, and `syntax.ml`.
