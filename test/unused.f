type maybe a
data constructor Nothing : forall a. {} -> maybe a
data constructor Just : forall a. { a } -> maybe a

program
  let const_nothing [a][b] (x : a) : maybe b =
    Nothing [b] {}
  in
  let test [c] (v : c) =
    const_nothing [c][c] v
  in
  test
