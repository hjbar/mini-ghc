type bool
data constructor True : {} -> bool
data constructor False : {} -> bool

type pair a b
data constructor Pair : forall a b. { a ; b } -> pair a b

program
  let f [a][b] (x : a) (y : b) = Pair [a][b] {x ; y} in
  f [bool] [bool] False {} True {}
