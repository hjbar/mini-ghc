type int
data constructor One : {} -> int

program
  fun (f : int -> int) (x : int) (y : int) =
    (f x) y
