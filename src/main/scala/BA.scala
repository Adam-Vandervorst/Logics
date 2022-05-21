import be.adamv.picograph.graphs.DNIELMG.{DNIELMG, given}
import be.adamv.picograph.graphs.PatchG.{C, PatchG, RewriteRule}
import be.adamv.picograph.nodeId

export be.adamv.picograph.algorithms.MetaRewriting

class BA[Prop]:
  val g = DNIELMG[Set[Prop]]()

  opaque type State = g.Node
  val init: State = g.newNode()

  inline def newState(): State = g.newNode()
  inline def states: Iterator[State] = g.nodes.iterator
  inline def proper_states: Iterator[State] = states.filter(_ != init)
  inline def connect(src: State, dst: State, props: Set[Prop]): Unit = g.connect(src, dst, props)

  def plot(): Unit =
    println(s"${init.nodeId} [peripheries=2];")
    for n <- proper_states do
      println(s"${n.nodeId};")
    for (s, ts) <- g.outgoing; t <- ts; l <- g.labeling(s, t) do
      println(s"${s.nodeId} -> ${t.nodeId} [label=\"$l\"];")


def rewriteSameOutgoing[A](g: DNIELMG[A]): Iterator[DNIELMG[A]] =
  val lhs = DNIELMG[Char]()
  val l = lhs.newNodes(2)

  val lpg = PatchG[Char, lhs.type](lhs)(List(C -> l(0), C -> l(1), l(0) -> l(0), l(1) -> l(1), l(0) -> C, l(1) -> C))

  val rhs = DNIELMG[Char]()
  val r = rhs.newNodes(1)

  val rpg = PatchG[Char, rhs.type](rhs)(List(C -> r(0), C -> r(0), r(0) -> r(0), r(0) -> C))

  RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 2, 4 -> 3, 5 -> 3)).applyIt(g)


def rewriteSameIncoming[A](g: DNIELMG[A]): Iterator[DNIELMG[A]] =
  val lhs = DNIELMG[Char]()
  val l = lhs.newNodes(2)

  val lpg = PatchG[Char, lhs.type](lhs)(List(C -> l(0), C -> l(1), l(0) -> l(0), l(1) -> l(1), l(0) -> C, l(1) -> C))

  val rhs = DNIELMG[Char]()
  val r = rhs.newNodes(1)

  val rpg = PatchG[Char, rhs.type](rhs)(List(C -> r(0), r(0) -> r(0), r(0) -> C, r(0) -> C))

  RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 0, 2 -> 1, 3 -> 1, 4 -> 2, 5 -> 3)).applyIt(g)


def rewriteIntoSelf[A](g: DNIELMG[A]): Iterator[DNIELMG[A]] =
  val lhs = DNIELMG[Char]()
  val l = lhs.newNodes(2)
  lhs.connect(l(0), l(1), 'a')
  lhs.connect(l(1), l(1), 'a')

  val lpg = PatchG[Char, lhs.type](lhs)(List(l(0) -> l(1), l(1) -> l(1),
                                             C -> l(0), C -> l(1),
                                             l(0) -> C, l(1) -> C))

  val rhs = DNIELMG[Char]()
  val r = rhs.newNodes(1)
  rhs.connect(r(0), r(0), 'a')

  val rpg = PatchG[Char, rhs.type](rhs)(List(r(0) -> r(0), C -> r(0), C -> r(0), r(0) -> C))

  RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 0, 2 -> 1, 3 -> 2, 4 -> 3, 5 -> 3)).applyIt(g)


def rewriteOutSelf[A](g: DNIELMG[A]): Iterator[DNIELMG[A]] =
  val lhs = DNIELMG[Char]()
  val l = lhs.newNodes(2)
  lhs.connect(l(0), l(0), 'a')
  lhs.connect(l(0), l(1), 'a')

  val lpg = PatchG[Char, lhs.type](lhs)(List(l(0) -> l(0), l(0) -> l(1),
                                             C -> l(0), C -> l(1),
                                             l(0) -> C, l(1) -> C))

  val rhs = DNIELMG[Char]()
  val r = rhs.newNodes(1)
  rhs.connect(r(0), r(0), 'a')

  val rpg = PatchG[Char, rhs.type](rhs)(List(r(0) -> r(0), C -> r(0), r(0) -> C, r(0) -> C))

  RewriteRule(lpg, rpg, Set(0 -> 0, 1 -> 0, 2 -> 1, 3 -> 1, 4 -> 2, 5 -> 3)).applyIt(g)
