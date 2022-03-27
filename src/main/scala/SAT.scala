def remove_pure(ps: Set[Set[Int]]): Set[Set[Int]] =
  val symbol_set = ps.flatten
  val pure_set = symbol_set.filterNot(e => symbol_set.contains(-e))
  ps.filter(p => (p intersect pure_set).isEmpty)

def remove_subsumed(ps: Set[Set[Int]]): Set[Set[Int]] =
  ps.filterNot(p => ps.exists(q => q < p))

private def update(ps: Set[Set[Int]], symbol: Int): Set[Set[Int]] =
  ps.filterNot(p => p.contains(symbol)).map(p => p.excl(-symbol))

def dpll(ps: Set[Set[Int]], solution: Set[Int] = Set()): Option[Set[Int]] =
  if ps.isEmpty then return Some(solution)
  if ps.exists(p => p.isEmpty) then return None
    
  for p <- ps; if p.size == 1; s <- p do
    return dpll(update(ps, s), solution.incl(s))

  val x: Int = ps.head.head
  dpll(update(ps, x), solution.incl(x)) orElse dpll(update(ps, -x), solution.incl(-x))
