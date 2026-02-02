open Atom
open Terms
open Types

(* ------------------------------------------------------------------------- *)

(* Specification of the eraser. *)

(* The eraser mainly consists of a single structurally recursive function
   that erases all joins and jumps by translating them into function
   definitions and function calls. *)

(* For the erasure mechanism to be sound, the program that will be erased
   must be optimized using the case-of-case optimization. *)

(* ------------------------------------------------------------------------- *)

(* The eraser *)

(* [erase xenv term] replaces all occurrences of join and jump in [term]
   with, respectively, function definitions and function calls.
   All typing information is reset; this means the term has to be
   type-checked again after erasure. *)
let rec erase (xenv : atom AtomMap.t) : fterm -> pre_fterm = function
  | TeVar (x, _) -> TeVar (x, reset ())
  | TeAbs (x, domain, body) -> TeAbs (x, domain, erase xenv body)
  | TeApp (term1, term2, _) ->
    TeApp (erase xenv term1, erase xenv term2, reset ())
  | TeLet (x, term1, term2) -> TeLet (x, erase xenv term1, erase xenv term2)
  | TeTyAbs (a, body) -> TeTyAbs (a, erase xenv body)
  | TeTyApp (term, typ_app, _) -> TeTyApp (erase xenv term, typ_app, reset ())
  | TeData (dc, dctyps, terms, _) ->
    TeData (dc, dctyps, List.map (erase xenv) terms, reset ())
  | TeTyAnnot (term, expected) -> TeTyAnnot (erase xenv term, expected)
  | TeMatch (term, return, clauses, _) ->
    let clauses =
      List.map
        (fun (Clause (PatData (loc, dc, typs, vars, _), term)) ->
          Clause (PatData (loc, dc, typs, vars, reset ()), erase xenv term) )
        clauses
    in
    TeMatch (erase xenv term, return, clauses, reset ())
  | TeLoc (loc, term) -> TeLoc (loc, erase xenv term)
  | TeJoin (j, typs, vars, expected, term1, term2) ->
    let j_name = j |> Atom.identifier |> Identifier.name in
    let x = Atom.fresh (Identifier.mk j_name Syntax.term_sort) in
    let inner_xenv = AtomMap.add j x xenv in

    let term1 =
      TeTyAnnot (erase xenv term1, expected)
      |> List.fold_right (fun (x, typ) acc -> TeAbs (x, typ, acc)) vars
      |> List.fold_right (fun a acc -> TeTyAbs (a, acc)) typs
    in

    let term2 = erase inner_xenv term2 in

    TeLet (x, term1, term2)
  | TeJump (j, typs, terms, expected) ->
    let x = AtomMap.find j xenv in
    let x = TeVar (x, reset ()) in

    let terms = List.map (erase xenv) terms in

    let call =
      List.fold_left (fun acc typ -> TeTyApp (acc, typ, reset ())) x typs
    in
    let call =
      List.fold_left (fun acc term -> TeApp (acc, term, reset ())) call terms
    in

    TeTyAnnot (call, expected)


(* [program prog] erases the program [prog].
   Requires [prog] to be optimized with case-of-case for the erasure
   to be sound. *)
let program (Prog (tctable, dctable, t) : program) : program =
  let pre_program = Prog (tctable, dctable, erase AtomMap.empty t) in
  ignore (Typecheck.run pre_program);
  Typecheck.petrify pre_program
