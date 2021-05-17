import scala.collection.immutable.SortedSet
import scala.reflect.ClassTag


def fix[A](f: A => A)(v: A, prev: Option[A] = None): A =
  if (prev.isEmpty || v != prev.get) fix(f)(f(v), Some(v))
  else v

sealed trait Prep extends ALogic

extension (t: AExpr[Prep])
  def eval(vars: Map[String, Boolean] = Map()): Boolean = t match
    case True => true
    case False => false
    case Var(s) => vars(s)
    case Not(p) => !p.eval(vars)
    case And(p, q) => p.eval(vars) && q.eval(vars)
    case Or(p, q) => p.eval(vars) || q.eval(vars)

  def to_dot(id: BigInt = 1): String = t match
    case True => s"$id [label=TRUE];\n"
    case False => s"$id [label=FALSE];\n"
    case Var(s) => s"$id [label=$s];\n"
    case Not(p) => s"$id [label=NOT];\n$id -> ${2*id};\n" ++
      p.to_dot(2*id)
    case And(p, q) => s"$id [label=AND];\n$id -> {${2*id}, ${2*id+1}};\n" ++
      p.to_dot(2*id) ++ q.to_dot(2*id+1)
    case Or(p, q) => s"$id [label=OR];\n$id -> {${2*id}, ${2*id+1}};\n" ++
      p.to_dot(2*id) ++ q.to_dot(2*id+1)

  private def reduce(): AExpr[Prep] = t match
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

  def simplify(): AExpr[Prep] = fix[AExpr[Prep]](_.reduce())(t)

  private def demorgan(): AExpr[Prep] = t match
    case Not(Or(p, q)) => Or(Not(p.demorgan()), Not(q.demorgan()))
    case Not(And(p, q)) => And(Not(p.demorgan()), Not(q.demorgan()))
    case Not(Not(v: Var)) => v
    case Not(p) => Not(p.demorgan())
    case And(p, q) => And(p.demorgan(), q.demorgan())
    case Or(p, q) => Or(p.demorgan(), q.demorgan())
    case x => x

  private def distribute(): AExpr[Prep] = t match
    case Or(p, And(q, r)) => And(Or(p, q), Or(p, r))
    case Or(And(p, q), r) => And(Or(p, r), Or(q, r))
    case Not(p) => Not(p.distribute())
    case And(p, q) => And(p.distribute(), q.distribute())
    case Or(p, q) => Or(p.distribute(), q.distribute())
    case x => x

  def cnf(index_map: Map[String, Int]): Set[Set[Int]] =
    val and_expr = fix[AExpr[Prep]](_.distribute())(fix[AExpr[Prep]](_.demorgan())(t))
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

case class Var(override val name: String) extends AVar[Prep](name)
case object True extends AAtom[Prep] with Show("1", "⊤", "true", "TRUE")
case object False extends AAtom[Prep] with Show("0", "⊥", "false", "FALSE")
case class Not(override val p: AExpr[Prep]) extends AUOp[Prep](p) with Show("!", "¬", "!", "NOT")
case class And(p: AExpr[Prep], q: AExpr[Prep]) extends ABOp[Prep](p, q) with Show(" * ", "∧", " && ", "AND")
case class Or(p: AExpr[Prep], q: AExpr[Prep]) extends ABOp[Prep](p, q) with Show(" + ", "∨", " || ", "OR")
