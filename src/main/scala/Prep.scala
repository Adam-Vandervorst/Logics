import scala.collection.immutable.SortedSet
import scala.reflect.ClassTag


def fix[A](f: A => A)(v: A, prev: Option[A] = None): A =
  if (prev.isEmpty || v != prev.get) fix(f)(f(v), Some(v))
  else v

enum Prep:
  case True
  case False
  case Var(name: String)
  case Not(p: Prep)
  case And(p: Prep, q: Prep)
  case Or(p: Prep, q: Prep)

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
    return (pairs.toMap, pairs.map(_.swap).toMap)

  def to_dot(id: BigInt = 1): String = this match
    case True => s"$id [label=TRUE];\n"
    case False => s"$id [label=FALSE];\n"
    case Var(s) => s"$id [label=$s];\n"
    case Not(p) => s"$id [label=NOT];\n$id -> ${2*id};\n" ++
      p.to_dot(2*id)
    case And(p, q) => s"$id [label=AND];\n$id -> {${2*id}, ${2*id+1}};\n" ++
      p.to_dot(2*id) ++ q.to_dot(2*id+1)
    case Or(p, q) => s"$id [label=AND];\n$id -> {${2*id}, ${2*id+1}};\n" ++
      p.to_dot(2*id) ++ q.to_dot(2*id+1)

  private def reduce(): Prep = this match
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

  def simplify(): Prep = fix[Prep](_.reduce())(this)

  private def demorgan(): Prep = this match
    case Not(Or(p, q)) => Or(Not(p.demorgan()), Not(q.demorgan()))
    case Not(And(p, q)) => And(Not(p.demorgan()), Not(q.demorgan()))
    case Not(Not(v: Var)) => v
    case Not(p) => Not(p.demorgan())
    case And(p, q) => And(p.demorgan(), q.demorgan())
    case Or(p, q) => Or(p.demorgan(), q.demorgan())
    case x => x

  private def distribute(): Prep = this match
    case Or(p, And(q, r)) => And(Or(p, q), Or(p, r))
    case Or(And(p, q), r) => And(Or(p, r), Or(q, r))
    case Not(p) => Not(p.distribute())
    case And(p, q) => And(p.distribute(), q.distribute())
    case Or(p, q) => Or(p.distribute(), q.distribute())
    case x => x

  def forrest[Flat <: Prep : ClassTag](): Set[Prep] = this match
    case p: Flat => p match
      case Not(a) => a.forrest[Flat]()
      case And(a, b) => a.forrest[Flat]() | b.forrest[Flat]()
      case Or(a, b) => a.forrest[Flat]() | b.forrest[Flat]()
      case x => Set(x)
    case q => Set(q)

  def cnf(index_map: Map[String, Int]): Set[Set[Int]] =
    val and_expr = fix[Prep](_.distribute())(fix[Prep](_.demorgan())(this))
    val clauses = and_expr.forrest[And]().map(_.forrest[Or]())
    val clean_clauses = clauses
      .filterNot(_.contains(True)) // remove true clauses
      .map(_.filterNot(_ == False)) // remove false variables
      .filterNot(_.isEmpty) // remove empty clauses
    val int_clauses = clean_clauses.map(_.map({
      case Not(Var(s)) => -index_map(s)
      case Var(s) => index_map(s)
      case _ => throw IllegalStateException()
    }))
    return int_clauses
