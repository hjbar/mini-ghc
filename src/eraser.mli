open Terms
open Types

(* The eraser replaces all occurrences of join and jump with function
   definitions and function calls in order to stay within the original
   fragment of the optimizer language. *)

(* Furthermore, the program that will be erased must be optimized with
   case-of-case to make the erasure procedure sound. *)

(* [program prog] erases the program [prog].
   Requires [prog] to be optimized with case-of-case for the erasure
   to be sound. *)

val program : program -> program
