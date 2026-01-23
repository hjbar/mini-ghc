type int

type pair a b
data constructor Pair : forall a b. { a ; b } -> pair a b

program
  let f [a][b] (x : a) (y : b) = Pair [a][b] {x ; y} in
  f [int]
