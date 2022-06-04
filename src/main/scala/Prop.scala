import scala.annotation.targetName
import scala.collection.immutable.SortedSet
import scala.reflect.ClassTag


enum Prop:
  case True
  case False
  case Var(name: String)
  case Not(p: Prop)
  case And(p: Prop, q: Prop)
  case Or(p: Prop, q: Prop)

  def eval(vars: Map[String, Boolean] = Map()): Boolean = this match
    case True => true
    case False => false
    case Var(s) => vars(s)
    case Not(p) => !p.eval(vars)
    case And(p, q) => p.eval(vars) && q.eval(vars)
    case Or(p, q) => p.eval(vars) || q.eval(vars)

  def vars(): SortedSet[String] = this match
    case True | False => SortedSet()
    case Var(s) => SortedSet(s)
    case Not(p) => p.vars()
    case And(p, q) => p.vars() | q.vars()
    case Or(p, q) => p.vars() | q.vars()

  def vars_mappings(): (Map[String, Int], Map[Int, String]) =
    val vars = this.vars()
    val pairs = vars.zip(1 to vars.size)
    (pairs.toMap, pairs.map(_.swap).toMap)

  def pretty(): String  = this match
    case True => "1"
    case False => "0"
    case Var(s) => s
    case Not(p) => "!" + p.pretty()
    case p: And => "(" + p.forrest[And]().map(_.pretty()).mkString(" * ") + ")"
    case p: Or => "(" + p.forrest[Or]().map(_.pretty()).mkString(" + ") + ")"

  def to_dot(id: BigInt = 1): String = this match
    case True => s"$id [label=TRUE];\n"
    case False => s"$id [label=FALSE];\n"
    case Var(s) => s"$id [label=$s];\n"
    case Not(p) => s"$id [label=NOT];\n$id -> ${2*id};\n" ++
      p.to_dot(2*id)
    case And(p, q) => s"$id [label=AND];\n$id -> {${2*id}, ${2*id+1}};\n" ++
      p.to_dot(2*id) ++ q.to_dot(2*id+1)
    case Or(p, q) => s"$id [label=OR];\n$id -> {${2*id}, ${2*id+1}};\n" ++
      p.to_dot(2*id) ++ q.to_dot(2*id+1)

  private def reduce(): Prop = this match
    case Not(Not(p)) => p.reduce()
    case Not(True) => False
    case Not(False) => True
    case Not(p) => Not(p.reduce())
    case And(False, _) => False
    case And(_, False) => False
    case And(True, q) => q.reduce()
    case And(p, True) => p.reduce()
    case And(p, q) if p == q => p.reduce()
    case And(p, Not(q)) if p == q => False
    case And(Not(p), q) if p == q => False
    case And(p, q) => And(p.reduce(), q.reduce())
    case Or(True, _) => True
    case Or(_, True) => True
    case Or(False, q) => q.reduce()
    case Or(p, False) => p.reduce()
    case Or(p, q) if p == q => p.reduce()
    case Or(p, Not(q)) if p == q => True
    case Or(Not(p), q) if p == q => True
    case Or(p, q) => Or(p.reduce(), q.reduce())
    case x => x

  def simplify(): Prop = fix[Prop](_.reduce())(this)

  private def demorgan(): Prop = this match
    case Not(Or(p, q)) => And(Not(p.demorgan()), Not(q.demorgan()))
    case Not(And(p, q)) => Or(Not(p.demorgan()), Not(q.demorgan()))
    case Not(Not(v: Var)) => v
    case Not(p) => Not(p.demorgan())
    case And(p, q) => And(p.demorgan(), q.demorgan())
    case Or(p, q) => Or(p.demorgan(), q.demorgan())
    case x => x

  private def distribute(): Prop = this match
    case Or(p, And(q, r)) => And(Or(p, q), Or(p, r))
    case Or(And(p, q), r) => And(Or(p, r), Or(q, r))
    case Not(p) => Not(p.distribute())
    case And(p, q) => And(p.distribute(), q.distribute())
    case Or(p, q) => Or(p.distribute(), q.distribute())
    case x => x

  def forrest[Flat <: Prop : ClassTag](): Set[Prop] = this match
    case p: Flat => p match
      case Not(a) => a.forrest[Flat]()
      case And(a, b) => a.forrest[Flat]() | b.forrest[Flat]()
      case Or(a, b) => a.forrest[Flat]() | b.forrest[Flat]()
      case x => Set(x)
    case q => Set(q)

  def cnf(index_map: Map[String, Int]): CNF =
    val and_expr = fix[Prop](_.distribute())(fix[Prop](_.demorgan())(this))
    val clauses = and_expr.forrest[And]().map(_.forrest[Or]())
    val clean_clauses = clauses
      .filterNot(_.contains(True)) // remove true clauses
      .map(_.filterNot(_ == False)) // remove false variables
    CNF(clean_clauses.map(_.map {
      case Not(Var(s)) => -index_map(s)
      case Var(s) => index_map(s)
      case _ => throw IllegalStateException()
    }))


object Prop:
  def Then(p: Prop, q: Prop): Prop = Or(Not(p), q)
  def Iff(p: Prop, q: Prop): Prop = And(Then(p, q), Then(q, p))


object CNFS:
  opaque type CNF = Set[Set[Int]]

  object CNF:
    def apply(ssi: Set[Set[Int]]): CNF = ssi

    def vacuous: CNF = Set()

    extension (cnf: CNF)
      def underlying: Set[Set[Int]] = cnf

      def isVacuous: Boolean = cnf.isEmpty

      def isUnsatisfiable: Boolean = cnf.exists(_.isEmpty)

      def units: Iterator[Int] = cnf.iterator.collect{ case p if p.size == 1 => p.head }.distinct

      def vars: Iterator[Int] = cnf.iterator.flatten.distinct

      def pureSplit: (Set[Int], CNF) =
        val symbol_set = cnf.flatten
        val pure_set = symbol_set.filterNot(e => symbol_set.contains(-e))
        (pure_set, cnf.filter(p => (p intersect pure_set).isEmpty))

      def withoutDual: CNF =
        cnf.filterNot(s => s.exists(x => s(-x)))

      def withoutSubsumed: CNF =
        cnf.filterNot(p => cnf.exists(q => q < p))

      def updated(symbol: Int): CNF =
        cnf.filterNot(p => p.contains(symbol)).map(p => p.excl(-symbol))
export CNFS.*


object DNFS:
  opaque type DNF = Set[Set[Int]]

  object DNF:
    def apply(ssi: Set[Set[Int]]): DNF = ssi

    extension (dnf: DNF)
      def underlying: Set[Set[Int]] = dnf

      def isVacuous: Boolean = dnf.exists(_.isEmpty)

      def blocks: Iterator[Set[Int]] = dnf.iterator

      def vars: Iterator[Int] = dnf.iterator.flatten.distinct

      def withoutImpure: DNF =
        dnf.map(s => s -- dnf.flatMap(t => t.map(-_) intersect s))

      def withoutSubsumed: DNF =
        dnf.filterNot(p => dnf.exists(q => q < p))
export DNFS.*
