import be.adamv.picograph.graphs.DNIELG.{DNIELG, given}
import be.adamv.picograph.graphs.DNIELMG.{DNIELMG, given}
import be.adamv.picograph.graphs.PatchG.{PatchG, DSL, RewriteRule}
import be.adamv.picograph.conversions.split
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

  def rewriteIntoSelf(): Unit =
    // NOTE: Demo
    import language.postfixOps
    import DSL.*
    val mg: DNIELMG[Set[Prop]] = g.split(Set(_))

    val lhs = DNIELMG[Char]()
    val l = lhs.newNodes(2)
    lhs.connect(l(0), l(1), 'a')
    lhs.connect(l(1), l(1), 'a')

    val (lpg, lpts) =
      given pg: PatchG[Char, lhs.type](lhs)
      (pg, Seq(<>-(l(0)), <>-(l(1))))

    val rhs = DNIELMG[Char]()
    val r = rhs.newNodes(1)
    rhs.connect(r(0), r(0), 'a')

    val (rpg, rpts) =
      given pg: PatchG[Char, rhs.type](rhs)
      (pg, Seq(<>-(r(0)), <>-(r(0))))

    val indexMap = Map(0 -> 0, 1 -> 1)
    val rr = RewriteRule(lpg, rpg)(indexMap.map((r, l) => rpts(r) -> lpts(l)))

    val todo1 = mg.occurrencesNaive(lhs).toList(1)
    val mg1 = rr.apply(mg, todo1)

    val todo2 = mg1.occurrencesNaive(lhs).toList(2)
    val mg2 = rr.apply(mg1, todo2)
    mg2.plot()
