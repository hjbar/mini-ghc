program
      fun [a][b] (e: a -> b)(v: a): b =
        (fun (x : a) = e x) v
