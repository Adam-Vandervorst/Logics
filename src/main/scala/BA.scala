import be.adamv.picograph.graphs.DNIELMG.{DNIELMG, given}
import be.adamv.picograph.graphs.PatchG.{PatchG, C, RewriteRule}
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


  def rewriteSameOutgoing(): Iterator[DNIELMG[Prop]] =
    val lhs = DNIELMG[Char]()
    val l = lhs.newNodes(2)

    val lpg = PatchG[Char, lhs.type](lhs)(List(C -> l(0), C -> l(1), l(0) -> l(0), l(1) -> l(1), l(0) -> C, l(1) -> C))

    val rhs = DNIELMG[Char]()
    val r = rhs.newNodes(1)

    val rpg = PatchG[Char, rhs.type](rhs)(List(C -> r(0), C -> r(0), r(0) -> r(0), r(0) -> C))

    RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 2, 4 -> 3, 5 -> 3)).applyIt(g)

  
  def rewriteSameIncoming(): Iterator[DNIELMG[Prop]] =
    val lhs = DNIELMG[Char]()
    val l = lhs.newNodes(2)

    val lpg = PatchG[Char, lhs.type](lhs)(List(C -> l(0), C -> l(1), l(0) -> l(0), l(1) -> l(1), l(0) -> C, l(1) -> C))

    val rhs = DNIELMG[Char]()
    val r = rhs.newNodes(1)

    val rpg = PatchG[Char, rhs.type](rhs)(List(C -> r(0), r(0) -> r(0), r(0) -> C, r(0) -> C))

    RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 0, 2 -> 1, 3 -> 1, 4 -> 2, 5 -> 3)).applyIt(g)


  def rewriteIntoSelf(): Iterator[DNIELMG[Prop]] =
    val lhs = DNIELMG[Char]()
    val l = lhs.newNodes(2)

    val lpg = PatchG[Char, lhs.type](lhs)(List(l(0) -> l(1), l(1) -> l(1),
                                               C -> l(0), C -> l(1),
                                               l(0) -> C, l(1) -> C))

    val rhs = DNIELMG[Char]()
    val r = rhs.newNodes(1)

    val rpg = PatchG[Char, rhs.type](rhs)(List(r(0) -> r(0), C -> r(0), C -> r(0), r(0) -> C))
    
    RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 0, 2 -> 1, 3 -> 2, 4 -> 3, 5 -> 3)).applyIt(g)


  def rewriteOutSelf(): Iterator[DNIELMG[Prop]] =
    val lhs = DNIELMG[Char]()
    val l = lhs.newNodes(2)

    val lpg = PatchG[Char, lhs.type](lhs)(List(l(0) -> l(0), l(0) -> l(1),
                                               C -> l(0), C -> l(1),
                                               l(0) -> C, l(1) -> C))

    val rhs = DNIELMG[Char]()
    val r = rhs.newNodes(1)

    val rpg = PatchG[Char, rhs.type](rhs)(List(r(0) -> r(0), C -> r(0), r(0) -> C, r(0) -> C))

    RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 0, 2 -> 1, 3 -> 1, 4 -> 2, 5 -> 3)).applyIt(g)
