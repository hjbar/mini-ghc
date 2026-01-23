type bool
data constructor True : {} -> bool
data constructor False : {} -> bool

type int
data constructor One : {} -> int

program
  fun (b : bool) =
    match b return int with
    | True {} -> One {}
    | False {} -> False {}
    end
