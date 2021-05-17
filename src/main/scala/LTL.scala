enum LTL:
  case True
  case False
  case Var(name: String)
  case Not(p: LTL)
  case And(p: LTL, q: LTL)
  case X(p: LTL)
  case U(p: LTL, q: LTL)
  
  def eval(path: List[Set[String]]): Boolean = this match
    case True => true
    case False => false
    case Var(s) => path(0).contains(s)
    case Not(p) => !p.eval(path)
    case And(p, q) => p.eval(path) && q.eval(path)
    case X(p) => p.eval(path.drop(1))
    case U(p, q) => (0 until path.size).find(i => q.eval(path.drop(i))) match
      case Some(i) => (0 until i).forall(j => p.eval(path.drop(j)))
      case None => false

object LTL:
  def Or(p: LTL, q: LTL): LTL = Not(And(Not(p), Not(q)))
  def F(p: LTL): LTL = U(True, p)
  def G(p: LTL): LTL = Not(F(Not(p)))
  def W(p: LTL, q: LTL): LTL = Or(U(p, q), G(p))
  def R(p: LTL, q: LTL): LTL = Not(U(Not(p), Not(q)))
