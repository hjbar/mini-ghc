type bool
data constructor True : {} -> bool
data constructor False : {} -> bool

type int
data constructor One : {} -> int

program
  let test (b : bool) : int =
    join j : bool = True {} in
    match b return int with
    | True {} -> jump j {} : int
    | False {} -> One {}
    end
  in test
