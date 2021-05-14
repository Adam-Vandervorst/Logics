extension[A](a: Set[A])
  def <=(b: Set[A]): Boolean = a.forall(b.contains)
  def <(b: Set[A]): Boolean = a.size < b.size && a <= b

def remove_pure(ps: Set[Set[Int]]): Set[Set[Int]] =
  val symbol_set = ps.flatten
  val pure_set = symbol_set.filterNot(e => symbol_set.contains(-e))
  return ps.filter(p => (p intersect pure_set).isEmpty)

def remove_subsumed(ps: Set[Set[Int]]): Set[Set[Int]] =
  return ps.filterNot(p => ps.exists(q => q < p))

private def update(ps: Set[Set[Int]], symbol: Int): Set[Set[Int]] =
  return ps.filterNot(p => p.contains(symbol)).map(p => p.excl(-symbol))

def dpll(ps: Set[Set[Int]], solution: Set[Int] = Set()): Option[Set[Int]] =
  if (ps.isEmpty) return Some(solution)
  if (ps.exists(p => p.isEmpty)) return None
    
  for (p <- ps; if p.size == 1; s <- p)
    return dpll(update(ps, s), solution.incl(s))

  val x: Int = ps.head.head
  return dpll(update(ps, x), solution.incl(x)) orElse dpll(update(ps, -x), solution.incl(-x))
