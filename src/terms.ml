(* This module defines the abstract syntax of Core, as produced
   by [Internalize]. *)

(* all kinds of names are represented by unique atoms *)

open Error
open Atom
open Types

(* ------------------------------------------------------------------------- *)

(* Internal type information. *)

(* When the type-checker runs, it decorates the term with type information.
   This information is then used during the simplification phase. *)

type 'a runtime = 'a option ref [@@deriving show]

let force (x : 'a runtime) : 'a = Option.get !x

let reset () : 'a runtime = ref None

(* At function applications, the type-checker records the following information: *)

type application_info = {
  (* The type of the argument. *)
  domain : ftype;
  (* The type of the result. *)
  codomain : ftype;
}
[@@deriving show]

let type_from_application_info info = TyArrow (info.domain, info.codomain)

(* At type applications, the type-checker records the following information: *)

type type_application_info = {
  (* The binder and type of a type abstraction *)
  gen : ftype_context;
}
[@@deriving show]

let type_from_type_application_info info = TyForall info.gen

(* On elimination forms of positive types, the type-checker records
   the following information: *)

type ftype_info = ftype [@@deriving show]

(* ------------------------------------------------------------------------- *)

(* Terms. *)

type ('a, 'b, 'c, 'd, 'e, 'f) _fterm =
  | TeVar of
      atom
      * (* variable name *)
      'a
  (* typechecker meta-data *)
  (* x *)
  | TeAbs of
      atom
      * (* function parameter *)
        ftype
      * (* function parameter's type *)
      ('a, 'b, 'c, 'd, 'e, 'f) _fterm (* function body *)
  | TeApp of
      ('a, 'b, 'c, 'd, 'e, 'f) _fterm
      * (* function *)
      ('a, 'b, 'c, 'd, 'e, 'f) _fterm
      * (* argument *)
      'b
  (* typechecker meta-data *)
  (* t t *)
  | TeLet of
      atom * ('a, 'b, 'c, 'd, 'e, 'f) _fterm * ('a, 'b, 'c, 'd, 'e, 'f) _fterm
    (* let x = t in t *)
  | TeTyAbs of atom * ('a, 'b, 'c, 'd, 'e, 'f) _fterm (* fun [ a ] = t *)
  | TeTyApp of ('a, 'b, 'c, 'd, 'e, 'f) _fterm * ftype * 'c
  (* typechecker meta-data *)
  (* t [ T ] *)
  | TeData of atom * ftype list * ('a, 'b, 'c, 'd, 'e, 'f) _fterm list * 'd
  (* typechecker meta-data *)
  (* K [ T ... T ] { t; ...; t } *)
  | TeTyAnnot of ('a, 'b, 'c, 'd, 'e, 'f) _fterm * ftype (* (t : T) *)
  | TeMatch of
      ('a, 'b, 'c, 'd, 'e, 'f) _fterm
      * ftype
      * ('a, 'b, 'c, 'd, 'e, 'f) _clause list
      * 'e
  (* typechecker meta-data *)
  (* match t return T with clause ... clause end *)
  | TeLoc of location * ('a, 'b, 'c, 'd, 'e, 'f) _fterm (* t *)
  | TeJoin of
      atom (* label j *)
      * atom list (* [ a ... a ] *)
      * (atom * ftype) list (* (x : T) ... (x : T) *)
      * ftype (* T *)
      * ('a, 'b, 'c, 'd, 'e, 'f) _fterm (* t *)
      * ('a, 'b, 'c, 'd, 'e, 'f) _fterm (* t *)
  (* join j [ a ... a ] (x : T) ... (x : T) : T = t in t *)
  | TeJump of
      atom (* label j *)
      * ftype list (* [ T ... T ] *)
      * ('a, 'b, 'c, 'd, 'e, 'f) _fterm list (* { t; ...; t } *)
      * ftype (* T *)
  (* jump j [ T ... T ] { t; ...; t } : T *)
  | TeLetRec of
      (atom * ftype * ('a, 'b, 'c, 'd, 'e, 'f) _fterm) list
      * ('a, 'b, 'c, 'd, 'e, 'f) _fterm
  (* lec rec (x : T) = t and (x : T) = t ... and (x : T) = t in t *)
  | TeJoinRec of
      ( atom
      * atom list
      * (atom * ftype) list
      * ftype
      * ('a, 'b, 'c, 'd, 'e, 'f) _fterm )
      list
      * ('a, 'b, 'c, 'd, 'e, 'f) _fterm
(* join rec j [ a ... a ] (x : T) ... (x : T) : T = t and j [ a ... a ] (x : T) ... (x : T) : T = t and ... j [ a ... a ] (x : T) ... (x : T) : T = t in t *)
(* the parser generates [TeLoc] nodes to keep track of locations within the source code. *)

and ('a, 'b, 'c, 'd, 'e, 'f) _clause =
  | Clause of 'f _pattern * ('a, 'b, 'c, 'd, 'e, 'f) _fterm
(* p -> t *)

and 'f _pattern = PatData of location * atom * atom list * atom list * 'f
(* typechecker meta-data *)
(* K [ a ... a ] { x; ...; x } *)
[@@deriving show, map]

(* ------------------------------------------------------------------------- *)

(* Utils for terms. *)

(* [get_xs defs] returns the list of let rec binded variables *)

let get_xs defs = List.map (fun (x, _, _) -> x) defs

(* [get_typed_xs] returns the list of the binded variables with their types *)

let get_typed_xs defs = List.map (fun (x, expected, _) -> (x, expected)) defs

(* [get_terms1 defs] returns the list of let rec term definitions *)

let get_terms1s defs = List.map (fun (_, _, terms1) -> terms1) defs

(* [get_js defs] returns the list of join rec binded labels *)

let get_js defs = List.map (fun (j, _, _, _, _) -> j) defs

(* [get_typss defs] returns the list of join rec types variables *)

let get_typss defs = List.map (fun (_, typs, _, _, _) -> typs) defs

(* [get_varss defs] returns the list of join rec typed variables *)

let get_varss defs = List.map (fun (_, _, vars, _, _) -> vars) defs

(* [get_expecteds defs] returns the list of join rec expected types *)

let get_expecteds defs = List.map (fun (_, _, _, expected, _) -> expected) defs

(* [get_expected defs] returns the expected type of join rec definitions
   The parser makes sure that the list of definitions is not empty *)

let get_expected defs =
  let _, _, _, expected, _ = List.hd defs in
  expected


(* ------------------------------------------------------------------------- *)

(* The type constructor table maps a type constructor to its arity. *)

type type_table = int AtomMap.t

(* ------------------------------------------------------------------------- *)

(* The data constructor table maps a data constructor to its type scheme. *)

(* As explained in [Types], for the sake of uniformity, we view type schemes
   as types. However, type schemes are types of a certain form:

      forall a ... a. { T; ... ; T } -> tc a ... a

   The tuple type constructor { T; ...; T } and the conjunction type constructor
   (T) are used only as part of type schemes, never as part of
   ordinary types. *)

type datacon_table = ftype AtomMap.t

(* ------------------------------------------------------------------------- *)

(* Programs. *)

type ('a, 'b, 'c, 'd, 'e, 'f) _program =
  | Prog of type_table * datacon_table * ('a, 'b, 'c, 'd, 'e, 'f) _fterm

(* ------------------------------------------------------------------------- *)

(* Shorthands *)

(* The petrifaction process (in [Typecheck]) turn a term with ['a
   runtime] metadata into a term with static ['a] metadata. We give a
   type synonym to easily denote these two phases. *)

(* Types before petrifaction *)

type pre_fterm =
  ( ftype runtime,
    application_info runtime,
    type_application_info runtime,
    ftype_info runtime,
    ftype_info runtime,
    ftype list runtime )
  _fterm
[@@deriving show]

type pre_clause =
  ( ftype runtime,
    application_info runtime,
    type_application_info runtime,
    ftype_info runtime,
    ftype_info runtime,
    ftype list runtime )
  _clause

type pre_program =
  ( ftype runtime,
    application_info runtime,
    type_application_info runtime,
    ftype_info runtime,
    ftype_info runtime,
    ftype list runtime )
  _program

(* Types after petrification *)

type fterm =
  ( ftype,
    application_info,
    type_application_info,
    ftype_info,
    ftype_info,
    ftype list )
  _fterm
[@@deriving show]

type clause =
  ( ftype,
    application_info,
    type_application_info,
    ftype_info,
    ftype_info,
    ftype list )
  _clause

type pattern = ftype list _pattern

type program =
  ( ftype,
    application_info,
    type_application_info,
    ftype_info,
    ftype_info,
    ftype list )
  _program
