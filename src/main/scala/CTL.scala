enum CTL:
  case True
  case False
  case Var(name: String)
  case Not(p: CTL)
  case And(p: CTL, q: CTL)

  case EX(p: CTL)
  case EG(p: CTL)
  case EU(p: CTL, q: CTL)


object CTL:
  def Or(p: CTL, q: CTL) = Not(And(Not(p), Not(q)))
  def Then(p: CTL, q: CTL) = Or(Not(p), q)
  def Iff(p: CTL, q: CTL) = And(Then(p, q), Then(q, p))
  def EF(p: CTL) = EU(True, p)
  def AX(p: CTL) = Not(EX(Not(p)))
  def AG(p: CTL) = Not(EF(Not(p)))
  def AF(p: CTL) = Not(EG(Not(p)))
  def AU(p: CTL, q: CTL) = Not(And(EU(Not(q), Not(And(p, q))), EG(Not(q))))
  