def dpll(ps: CNF, solution: Set[Int] = Set()): Option[Set[Int]] =
  if ps.isVacuous then return Some(solution)
  if ps.isUnsatisfiable then return None

  for s <- ps.units do
    return dpll(ps.updated(s), solution.incl(s))

  val x: Int = ps.vars.next()
  dpll(ps.updated(x), solution.incl(x)) orElse dpll(ps.updated(-x), solution.incl(-x))
