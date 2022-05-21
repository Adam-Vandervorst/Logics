import Prop.False

enum LTL:
  case True
  case False
  case Var(name: String)
  case Not(p: LTL)
  case And(p: LTL, q: LTL)
  case Or(p: LTL, q: LTL)
  case Next(p: LTL)
  case Until(p: LTL, q: LTL)
  case Release(p: LTL, q: LTL)
  
  def show(): String  = this match
    case True => "True"
    case False => "False"
    case Var(s) => s"\"$s\""
    case Not(p: Not) => s"!(${p.show()})"
    case Not(p) => s"!${p.show()}"
    case And(p, q) => s"(${p.show()} /\\ ${q.show()})"
    case Or(p, q) => s"(${p.show()} \\/ ${q.show()})"
    case Next(p) => s"X(${p.show()})"
    case Until(p, q) => s"(${p.show()} U ${q.show()})"
    case Release(p, q) => s"(${p.show()} R ${q.show()})"

  def eval(path: List[Set[String]]): Boolean = this match
    case True => true
    case False => false
    case Var(s) => path.head.contains(s)
    case Not(p) => !p.eval(path)
    case And(p, q) => p.eval(path) && q.eval(path)
    case Or(p, q) => Not(And(Not(p), Not(q))).eval(path)
    case Next(p) => p.eval(path.drop(1))
    case Until(p, q) => path.indices.find(i => q.eval(path.drop(i))) match
      case Some(i) => (0 until i).forall(j => p.eval(path.drop(j)))
      case None => false
    case Release(p, q) => Not(Until(Not(p), Not(q))).eval(path)

  def mapped(f: LTL => LTL): LTL = this match
    case Not(p) => Not(f(p))
    case And(p, q) => And(f(p), f(q))
    case Or(p, q) => Or(f(p), f(q))
    case Next(p) => Next(f(p))
    case Until(p, q) => Until(f(p), f(q))
    case Release(p, q) => Release(f(p), f(q))
    case x => x

  private def distribute_not(): LTL = this match
    case Not(Not(v: Var)) => v
    case Not(And(p, q)) => Or(Not(p.distribute_not()), Not(q.distribute_not()))
    case Not(Or(p, q)) => And(Not(p.distribute_not()), Not(q.distribute_not()))
    case Not(Next(p)) => Next(Not(p))
    case Not(Until(p, q)) => Release(Not(p), Not(q))
    case Not(Release(p, q)) => Until(Not(p), Not(q))
    case x => x.mapped(_.distribute_not())

  def nnf(): LTL = fix[LTL](_.distribute_not())(this).simplify()

  private def reduce(): LTL = this match
    case Not(Not(p)) => p.reduce()
    case Not(True) => False
    case Not(False) => True
    case And(False, _) => False
    case And(_, False) => False
    case And(True, q) => q.reduce()
    case And(p, True) => p.reduce()
    case And(p, q) if p == q => p.reduce()
    case And(p, Not(q)) if p == q => False
    case And(Not(p), q) if p == q => False
    case Or(True, _) => True
    case Or(_, True) => True
    case Or(False, q) => q.reduce()
    case Or(p, False) => p.reduce()
    case Or(p, q) if p == q => p.reduce()
    case Or(p, Not(q)) if p == q => True
    case Or(Not(p), q) if p == q => True
    case Next(True) => True
    case Next(False) => False
    case And(Next(p), Next(q)) => Next(And(p.reduce(), q.reduce()))
    case Until(Next(p), Next(q)) => Next(Until(p.reduce(), q.reduce()))
    case Release(Next(p), Next(q)) => Next(Release(p.reduce(), q.reduce()))
    case Until(p, False) => False
    case And(p1, Until(q, p2)) if p1 == p2 => p1.reduce()
    case And(p1, Release(q, p2)) if p1 == p2 => p1.reduce()
    case And(Release(p1, r), Release(p2, s)) if p1 == p2 => Release(p1.reduce(), And(r.reduce(), s.reduce()))
    case Or(Release(r, p1), Release(s, p2)) if p1 == p2 => Release(Or(r.reduce(), s.reduce()), p1.reduce())
    case x => x.mapped(_.reduce())

  def simplify(): LTL = fix[LTL](_.reduce())(this)

  def atomics(): Set[LTL] = this match
    case True => Set()
    case False => Set()
    case Var(s) => Set(Var(s))
    case Not(p) => p.atomics()
    case And(p, q) => p.atomics() ++ q.atomics()
    case Or(p: LTL, q: LTL) => p.atomics() ++ q.atomics()
    case Next(p: LTL) => p.atomics()
    case Until(p: LTL, q: LTL) => p.atomics() ++ q.atomics()
    case Release(p: LTL, q: LTL) => p.atomics() ++ q.atomics()

  def subformulas(): LazyList[LTL] = this #:: (this match
    case True => LazyList()
    case False => LazyList()
    case Var(s) => LazyList()
    case Not(p) => p.subformulas()
    case And(p, q) => p.subformulas() ++ q.subformulas()
    case Or(p: LTL, q: LTL) => p.subformulas() ++ q.subformulas()
    case Next(p: LTL) => p.subformulas()
    case Until(p: LTL, q: LTL) => p.subformulas() ++ q.subformulas()
    case Release(p: LTL, q: LTL) => p.subformulas() ++ q.subformulas())

  def neg: LTL = this match
    case Not(p) => p
    case p => Not(p)

object LTL:
  def fairness(n: Int): LTL =
    val ps = (1 to n).map(k => Globally(Finally(Var(s"p$k"))))
    Not(Then(ps.reduce(And(_, _)), Globally(Then(Var("q"), Finally(Var("r"))))))
  
  def Finally(p: LTL): LTL = Until(True, p)
  def Globally(p: LTL): LTL = Not(Finally(Not(p)))
  def Weak(p: LTL, q: LTL): LTL = Release(q, Or(p, q))
  def Strong(p: LTL, q: LTL): LTL = Until(q, And(p, q))
  def Then(p: LTL, q: LTL): LTL =  Or(Not(p), q)
  def Iff(p: LTL, q: LTL): LTL = And(Then(p, q), Then(q, p))

  object Literal:
    def apply(name: String): LTL = if name.head == '!'
      then Not(Var(name.tail))
      else Var(name)  
    def unapply(f: LTL): Option[String] = f match
      case Var(p) => Some(p)
      case Not(Var(p)) => Some(s"!$p")
      case _ => None

  object DSL:
    given Conversion[String, Var] = Var.apply

    def X(p: LTL): LTL = Next(p)
    def F(p: LTL): LTL = Finally(p)
    def G(p: LTL): LTL = Globally(p)

    extension (p: LTL)
      def unary_! : LTL = Not(p)
      def ==>(q: LTL) = Then(p, q)
      def <=>(q: LTL) = Iff(p, q)
      def /\(q: LTL) = And(p, q)
      def \/(q: LTL) = Or(p, q)
      infix def U(q: LTL) = Until(p, q)
      infix def R(q: LTL) = Release(p, q)
      infix def W(q: LTL): LTL = Weak(p, q)
      infix def M(q: LTL): LTL = Strong(p, q)
