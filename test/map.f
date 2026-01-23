type bool
data constructor True : {} -> bool
data constructor False : {} -> bool

type list a
data constructor Nil : forall a. {} -> list a
data constructor Cons : forall a. { a ; list a } -> list a

program
  let map [a][b] (f : a -> b) (l : list a) : list b =
    match l return list b with
    | Nil [_] {} -> Nil [b] {}
    | Cons [_] {x ; l} ->
        let x' = f x in
        Cons [b] {x' ; Nil [b] {}}
    end
  in
  map
