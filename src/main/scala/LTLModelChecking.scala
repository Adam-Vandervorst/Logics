import scala.collection.mutable
import LTL.*


def create_graph(formula: LTL, plot: Boolean = false): BA[String] =
  val ba = BA[String]()
  val now = mutable.Map[ba.State, Set[LTL]]()
  val following = mutable.Map[ba.State, Set[LTL]]()

  def expand(curr: Set[LTL], old: Set[LTL], next: Set[LTL], parent: ba.State): Unit =
    if curr.isEmpty then
      if plot then {println("digraph {"); ba.plot(); println("}")}
      val labels = old.collect{ case Literal(n) => n }
      ba.proper_states.find(q => following(q) == next && now(q) == old) match
        case Some(q) => ba.connect(parent, q, labels)
        case None =>
          val node = ba.newState()
          ba.connect(parent, node, labels)
          following(node) = next
          now(node) = old
          expand(next, Set(), Set(), node)
    else
      val f = curr.head
      val curr_ = curr.tail
      val old_ = old + f
      f match
        case True | False | Var(_) | Not(Var(_)) =>
          if f == False || old.contains(f.neg) then ()
          else expand(curr_, old_, next, parent)
        case And(p, q) =>
          expand(curr_ + p + q -- old_, old_, next, parent)
        case Next(p) =>
          expand(curr_, old_, next + p, parent)
        case Or(p, q) =>
          expand(curr_ + p -- old_, old_, next, parent)
          expand(curr_ + q -- old_, old_, next, parent)
        case Until(p, q) =>
          expand(curr_ + p -- old_, old_, next + Until(p, q), parent)
          expand(curr_ + q -- old_, old_, next, parent)
        case Release(p, q) =>
          expand(curr_ + q -- old_, old_, next + Release(p, q), parent)
          expand(curr_ + p + q -- old_, old_, next, parent)

  expand(Set(formula), Set(), Set(), ba.init)

  val us = formula.subformulas().collect{
    case u @ Until(p, q) =>
      ba.proper_states.filter(n => !now(n).contains(u) || now(n).contains(q)).toSet
  }.toSet
  val finals = if us.nonEmpty then us else Set(ba.states.toSet)

  if plot then
    println("digraph {"); ba.plot(); println("}")
  else
    println(s"nnf: $formula")
    println(s"fin: $finals")
    println(s"now: $now")

  ba


@main def buchi_test =
  import LTL.DSL.{*, given}
//  println(create_graph(X(X("p") U "q")))
//  println(create_graph(F(X(X("p") U "q")).nnf()))
//  println(create_graph(Next(Var("p"))))
//  println(create_graph(Or(Next(Var("p")), Next(Var("q")))))
//  println(create_graph(G("p" \/ X("q")).nnf()))
//  println(create_graph(((False R (True U "p1")) /\ (True U ("q" /\ (False R !"r"))))))
//  println(create_graph("p" U "q"))
//  println(create_graph("p" U X("q")))
//  println(create_graph(F(G("p"))))
//  val ba = create_graph((F("p") \/ F("q")).nnf())
//  ba.plot()
//  println(create_graph(G("p" \/ X("q")).nnf()))
//  val ba = create_graph((!(G(F("p1")) ==> G("q" ==> F("r")))).nnf())
//  ba.plot()
//  println((!(G(F("p1")) ==> G("q" ==> F("r")))).nnf().show())
//  val ba = create_graph((F(X(X("p") U "q"))).nnf())
//  ba.plot()
//  val ba = create_graph((G("p" \/ X("q"))).nnf())
//  ba.plot()
//  val ba = create_graph((("p" U "q") /\ ("r" U "s")).nnf())
//  ba.plot()
  val ba = create_graph(("a" U ("b" U "c")).nnf())
//  ba.plot()
  ba.rewriteIntoSelf()
//  val ba = create_graph((G((!"passport" \/ !"ticket") ==> X(!"board"))).nnf())
//  ba.plot()
//  val ba = create_graph(("a" ==> X("b" ==> "c")).nnf())
//  ba.plot()
//  val ba = create_graph((("a" \/ "b") \/ ("a" /\ "b")).nnf())
//  ba.plot()
