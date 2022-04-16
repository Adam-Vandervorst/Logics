import scala.collection.mutable

class BA[Label]:
  opaque type State = Int
  val adjacency: mutable.Map[State, Set[State]] = mutable.Map()
  val labeling: mutable.Map[(State, State), Set[Label]] = mutable.Map().withDefaultValue(Set())
  private var next_id = 0

  val init: State = newNode()

  def newNode(): State =
    val node = next_id
    next_id += 1
    adjacency(node) = Set()
    node

  def connect(src: State, dst: State, labels: Set[Label]): Unit =
    adjacency(src) += dst
    labeling((src, dst)) ++= labels

  def states: Iterable[State] = adjacency.keys
  def proper_states: Iterable[State] = adjacency.keys.filter(_ > 0)
  def sinks: Iterable[State] = adjacency.collect{ case (k, vs) if vs == Set(k) => k }

  def plot(): Unit =
    for (n, nbs) <- adjacency
        b <- nbs do
      println(s"$n -> $b [label=\"${labeling((n, b)).mkString(" ")}\"];")

  def incoming(s: State): Set[State] =
    adjacency.collect{case (k, vs) if vs.contains(s) => k}.toSet

  /*
  case class Merge(a: State, b: State)
  def rewrite(): Unit =
    for a <- states
        b <- states
        if a < b && adjacency(a) == adjacency(b)
        if (incoming(a) intersect incoming(b)).forall(i => labeling(i, a) == labeling(i, b))
    yield
      Merge(a, b)
   */
