program
  fun [a] (x : a) =
    let f = fun [b] (z : b) = z in
    fun (y : b) = x
