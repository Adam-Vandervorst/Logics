import scala.collection.mutable

type Var = String

case class State(name: String, variables: Set[Var], initial: Boolean = false)

class Kripke():
  val adjacency: mutable.Map[State, Set[State]] = mutable.Map()

  def states: Iterable[State] = adjacency.keys

  def initial_states: Iterable[State] = states.filter(_.initial)

  def connect(x: State, y: State): Unit =
    val new_val = adjacency.get(x) match
      case Some(s) => s.incl(y)
      case None => Set(y)
    adjacency.update(x, new_val)
    adjacency.getOrElseUpdate(y, Set())

  def path(circular: Boolean, xs: State*): Unit =
    xs.reduce((a, b) => {connect(a, b); b})
    if (circular) connect(xs.last, xs.head)

  def add_reflexive(): Unit =
    adjacency.mapValuesInPlace((s, ss) => ss.incl(s))
  
  def to_dot(): String =
    (Seq("node [shape=Mrecord]") ++
     adjacency.keys.map(s => s"${s.name} [label=\"{${s.name} | {${s.variables.mkString(" | ")}}}\"${if (s.initial) ", style=bold" else ""}]") ++
     adjacency.map((s, ss) => s"${s.name} -> {${ss.map(_.name).mkString("; ")}}") ++
     Seq("")).mkString(";\n")
