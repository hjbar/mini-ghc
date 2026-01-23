type int
data constructor One : {} -> int

type list a
data constructor Nil : forall a. {} -> list a
data constructor Cons : forall a. { a ; list a } -> list a

program
  Cons [int] { One {} }
