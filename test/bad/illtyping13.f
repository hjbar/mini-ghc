type maybe a
data constructor Nothing : forall a. {} -> maybe a

type list a
data constructor Nil : forall a. {} -> list a

program
  fun [a] (l : list a) =
    match l return maybe a with
    | Nothing [_] {} -> Nothing [a] {}
    end
