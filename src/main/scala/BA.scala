import be.adamv.picograph.graphs.DNIELMG.{DNIELMG, given}
import be.adamv.picograph.graphs.PatchG.{PatchG, DSL, RewriteRule}
import be.adamv.picograph.conversions.split
import be.adamv.picograph.nodeId


class BA[Prop]:
  val g = DNIELMG[Prop]()

  opaque type State = g.Node
  val init: State = g.newNode()

  inline def newState(): State = g.newNode()
  inline def states: Iterator[State] = g.nodeIt
  inline def proper_states: Iterator[State] = states.filter(_ != init)
  inline def connect(src: State, dst: State, props: Set[Prop]): Unit = props.foreach(prop => g.connect(src, dst, prop))

  def plot(): Unit =
    println(s"${init.nodeId} [peripheries=2];")
    for n <- proper_states do
      println(s"${n.nodeId};")
    for (s, t) <- g.edgeIt do
      println(s"${s.nodeId} -> ${t.nodeId} [label=\"${g.labeling(s, t).mkString(" ")}\"];")

  def rewriteIntoSelf(): Iterator[DNIELMG[Prop]] =
    import language.postfixOps
    import DSL.*
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
    RewriteRule(lpg, rpg)(indexMap.map((r, l) => rpts(r) -> lpts(l)))
      .applyIt(g)
