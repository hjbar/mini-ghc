type pair a b
data constructor Pair : forall a b. { a ; b } -> pair a b

type maybe a
data constructor Just : forall a. { a } -> maybe a

program
  fun [a][b] (e: a -> b)(v: a): pair (maybe b) (maybe b) =
    (fun (x : a) = Pair [maybe b] [maybe b] { Just [b] { e x } ; Just [b] { e x } }) v
