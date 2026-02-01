type t1
data constructor A : {} -> t1
data constructor B : {} -> t1

type t2
data constructor C : {} -> t2
data constructor D : {} -> t2

type t3
data constructor E : {} -> t3
data constructor F : {} -> t3

type t4
data constructor G : {} -> t4
data constructor H : {} -> t4

program
  match
    (match
      (match (A {} : t1) return t2 with
       | A {} -> C {}
       | B {} -> D {}
       end)
    return t3 with
    | C {} -> E {}
    | D {} -> F {}
    end)
  return t4 with
  | E {} -> G {}
  | F {} -> H {}
  end
