@main def print_expr =
  val (c, t) =
    ("colored",
     "textured")
  val (q0, q1, q2) =
    (State("q0", Set(c, t), true),
     State("q1", Set(t)),
     State("q2", Set(c)))

  val M = Kripke()

  M.path(false, q0, q1, q2)
  M.add_transitive()
  M.add_reflexive()
  
  println(M.pretty())
