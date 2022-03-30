import CTL.*

def CTLCheck(model: Kripke, formula: CTL): Set[State] =
  formula match
    case True => model.states.toSet
    case False => Set()
    case Var(s) => model.states.filter(_.variables.contains(s)).toSet
    case Not(p) => model.states.filterNot(CTLCheck(model, p)).toSet
    case And(p, q) => CTLCheck(model, p).intersect(CTLCheck(model, q))
    case EX(p) => model.ex_preimg(CTLCheck(model, p)).toSet
    case EG(p) =>
      var Q1 = model.states.toSet
      var Q2 = CTLCheck(model, p)
      while !Q1.subsetOf(Q2) do
        Q1 = Q2
        Q2 = model.ex_preimg(Q1).toSet.intersect(Q1)
      Q1
    case EU(p, q) =>
      var Q1 = Set[State]()
      var Q2 = CTLCheck(model, q)
      while !Q2.subsetOf(Q1) do
        Q1 = Q1.union(Q2)
        Q2 = model.ex_preimg(Q1).toSet.intersect(CTLCheck(model, p))
      Q1
