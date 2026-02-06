open Atom
open Types
open Terms

(* ------------------------------------------------------------------------- *)

(* [fv term] is the set of the free term variables of [term]. *)

let sremove (sa : atom list) (s : AtomSet.t) : AtomSet.t =
  List.fold_right AtomSet.remove sa s


let union_map (f : 'a -> AtomSet.t) (xs : 'a list) : AtomSet.t =
  List.fold_left (fun accu x -> AtomSet.union accu (f x)) AtomSet.empty xs


let rec fv = function
  | TeVar (x, _) -> AtomSet.singleton x
  | TeAbs (x, _, body) -> AtomSet.remove x (fv body)
  | TeApp (term1, term2, _) -> AtomSet.union (fv term1) (fv term2)
  | TeLet (x, term1, term2) ->
    AtomSet.union (fv term1) (AtomSet.remove x (fv term2))
  | TeTyAbs (_, term)
   |TeTyApp (term, _, _)
   |TeTyAnnot (term, _)
   |TeLoc (_, term) ->
    fv term
  | TeData (_, _, fields, _) -> union_map fv fields
  | TeMatch (term, _, clauses, _) ->
    AtomSet.union (fv term) (union_map fv_clause clauses)
  | TeJoin (j, _, vars, _, term1, term2) ->
    let xs = List.map fst vars in
    let fv1 = sremove xs (fv term1) in

    let fv2 = AtomSet.remove j (fv term2) in

    AtomSet.union fv1 fv2
  | TeJump (j, _, fields, _) -> AtomSet.add j (union_map fv fields)
  | TeLetRec (defs, term2) ->
    let fv1 = union_map fv (get_terms1s defs) in
    sremove (get_xs defs) (AtomSet.union fv1 (fv term2))
  | TeJoinRec (defs, term2) ->
    let fv1 =
      union_map
        (fun (_, _, vars, _, term1) ->
          let xs = List.map fst vars in
          sremove xs (fv term1) )
        defs
    in
    sremove (get_js defs) (AtomSet.union fv1 (fv term2))


and fv_clause = function
  | Clause (PatData (_, _, _, tevars, _), term) -> sremove tevars (fv term)


(* ------------------------------------------------------------------------- *)

(* [head] extracts the type constructor that lies at the head of a type
   scheme. *)

let rec head = function
  | TyBoundVar _ | TyFreeVar _ | TyTuple _ -> assert false
  | TyArrow (_, ty) -> head ty
  | TyForall body ->
    let dummy = TyBoundVar 0 in
    head (fill body dummy)
  | TyCon (tc, _) -> tc


let type_scheme (Prog (_, dctable, _)) dc = AtomMap.find dc dctable

let type_constructor p dc =
  (* Find the type scheme associated with [dc], and go down into it
     to extract its head. This is inefficient, but good enough for us,
     and avoids the need to build a redundant table. *)
  head (type_scheme p dc)


let data_constructors (Prog (_, dctable, _)) tc =
  (* Iterate over all data constructors, and find those that are associated
     with [tc]. This is inefficient, but good enough for us, and avoids the
     need to build a redundant table. *)
  AtomMap.fold
    (fun dc s dcs -> if Atom.equal tc (head s) then AtomSet.add dc dcs else dcs)
    dctable AtomSet.empty
