type pair a b
data constructor Pair : forall a b. { a ; b } -> pair a b

program
  fun [a][b](v: a)(c: a -> b): pair b b =
    let x : a = v in
    Pair [b][b] { c x ; c x }
