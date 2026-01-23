type pair a b
data constructor Pair : forall a b. { a ; b } -> pair a b

program
  let swap [a][b] (p : pair a b) : pair b a =
    match p return pair b a with
    | Pair [a][b] {x ; y} -> Pair [b][a] {y ; x}
    end
  in
  swap
