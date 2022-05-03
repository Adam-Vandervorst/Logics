import be.adamv.picograph.graphs.DNIELG.{DNIELG, given}
import be.adamv.picograph.{Rooted, nodeId}

class BA[Prop]:
  private val g = DNIELG[Set[Prop]]()
  private given r: Rooted[g.type, State] with
    extension (ev: g.type)
      def root: State = init

  opaque type State = g.Node
  val init: State = g.newNode()

  inline def newState(): State = g.newNode()
  inline def states: Iterator[State] = g.nodeIt
  inline def proper_states: Iterator[State] = r.nonRootNodeIt(g.asInstanceOf)
  inline def connect(src: State, dst: State, props: Set[Prop]): Unit = g.connect(src, dst, props)

  def plot(): Unit =
    println(s"${init.nodeId} [peripheries=2];")
    for n <- r.nonRootNodeIt(g) do
      println(s"${n.nodeId};")
    for (s, ls, t) <- g.edgeIt do
      println(s"${s.nodeId} -> ${t.nodeId} [label=\"${ls.mkString(" ")}\"];")
