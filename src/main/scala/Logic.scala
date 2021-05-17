import scala.collection.immutable.SortedSet
import scala.reflect.ClassTag


trait AExpr[Logic <: ALogic]:
  def vars(): SortedSet[String] = this match
    case _: AAtom[Logic] => SortedSet()
    case v: AVar[Logic] => SortedSet(v.name)
    case op: AOp[Logic] => op.toList.map(_.vars()).reduce(_ | _)

  def vars_mappings(): (Map[String, Int], Map[Int, String]) =
    val vars = this.vars()
    val pairs = vars.zip(1 to vars.size)
    return (pairs.toMap, pairs.map(_.swap).toMap)

  def forrest[Flat <: AOp[Logic] : ClassTag](): Set[AExpr[Logic]] = this match
    case op: Flat => op.map(_.forrest[Flat]()).reduce(_ | _)
    case q => Set(q)

  def pretty()(using stype: SType = SType.Algebra): String  = this match
    case a: AAtom[Logic] => "X"
    case v: AVar[Logic] => v.name
    case op: AUOp[Logic] => "!" + op.p.pretty()
    case op: ABOp[Logic] => "(" + op.forrest[op.type]().map(p => p.pretty()).mkString("|") + ")"

trait AVar[Logic <: ALogic](val name: String) extends AExpr[Logic]

trait AAtom[Logic <: ALogic] extends AExpr[Logic]

abstract trait AOp[Logic <: ALogic] extends AExpr[Logic] with Iterable[AExpr[Logic]]

trait AUOp[Logic <: ALogic](val p: AExpr[Logic]) extends AOp[Logic]:
  def iterator = List(p).iterator
  def toString: String

trait ABOp[Logic <: ALogic](p: AExpr[Logic], q: AExpr[Logic]) extends AOp[Logic]:
  def iterator = List(p, q).iterator
  def toString: String

trait AMOp[Logic <: ALogic](ps: AExpr[Logic]*) extends AOp[Logic]:
  def iterator = ps.iterator
  def toString: String

abstract trait ALogic
