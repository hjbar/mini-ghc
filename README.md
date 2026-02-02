# Work completed

I completed all the mandatory tasks in addition to the following extensions:

# Extensions

## Type-preserving erasure

The goal of this extension is to obtain, from a ```μCoreJ``` program, a ```μCore``` program that preserves both the typing and the semantics of the original ```μCoreJ``` program. To achieve this, all occurrences of ```jump``` in the ```μCoreJ``` program must be in tail position. In order to obtain a ```μCoreJ``` program in which all occurrences of ```jump``` are in tail position, the optimization rules ```commute``` and ```abort``` must be applied as much as possible. Once this is done, all ```jumps``` will be in tail position (as proven in the paper Luke Maurer, Paul Downen, Zena M. Ariola, and Simon L. Peyton Jones. 2017. Compiling without continuations). This is convenient because the mandatory tasks require implementing these two rules, which are essential for the soundness of erasure. One can then replace all ```joins``` with function definitions and all ```jumps``` with calls to these functions.

To implement this, I ensure that all calls to the erasure procedure are performed on programs optimized with ```case-of-case``` (to satisfy the conditions required for the soundness of erasure). For the erasure itself, I use a function that removes all ```joins``` and ```jumps``` from a given typed term. Since the program must be optimized beforehand with ```case-of-case```, we can simply replace all ```joins``` and ```jumps``` without further concern. One detail is that this function returns an untyped term, because we do not know what information to provide when constructing the function call that replaces a ```jump```. Finally, I have a function that erases a program: it takes a typed program, erases its ```joins``` and ```jumps```, then type-checks it (to obtain a typed program as output) and returns it.

To test the erasure, I optimize all the test files using ```case-of-case```, then I compare the obtained result with the expected result.

To ensure that erasure is always applied to a program optimized with ```case-of-case```, this is handled in the file ```joujou.ml```. The implementation of the erasure itself is located in the file ```eraser.ml```. The test files are located in the ```test``` directory.
