type int
data constructor One : {} -> int

program
  let x = One {} in
  x [int]
