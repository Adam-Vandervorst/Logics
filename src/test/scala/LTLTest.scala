import scala.collection.immutable.SortedSet
import org.scalatest.funsuite.AnyFunSuite
import LTL._


class LTLTest extends AnyFunSuite:
  import LTL.DSL.{*, given}

  val p1 = List(Set("a", "b"), Set("b"), Set("b", "c"), Set("b", "c"))
  val e1 = G("b")
  val e2 = F(G("c"))
  val e3 = F(!G("a") /\ X("c"))
  val e4 = G("a" \/ "c")
  val e5 = X("a")
  val e6 = "a" R "c"

  def fairness(n: Int): LTL =
    val ps = (1 to n).map(k => G(F(s"p$k")))
    !(ps.reduce(_ /\ _) ==> G("q" ==> F("r")))

  test("p1 positive eval") {
    assert(e1.eval(p1))
    assert(e2.eval(p1))
    assert(e3.eval(p1))
  }

  test("p1 negative eval") {
    assert(!e4.eval(p1))
    assert(!e5.eval(p1))
    assert(!e6.eval(p1))
  }

  test("show") {
    assert((!("p" R "q") /\ !"s").show() == "(!(\"p\" R \"q\") /\\ !\"s\")")
    assert((!("p" U ("q" \/ "s")) U ("p" R "s")).show() == "(!(\"p\" U (\"q\" \\/ \"s\")) U (\"p\" R \"s\"))")
    assert(((X(X("p")) /\ !X("p")) \/ !(!"q")).show() == "((X(X(\"p\")) /\\ !X(\"p\")) \\/ !(!\"q\"))")
    assert(((True R "long1") /\ (False U "long2")).show() == "((True R \"long1\") /\\ (False U \"long2\"))")
  }

  test("fairness") {
    assert(!(G(F("p1")) ==> G("q" ==> F("r"))) == fairness(1))
    assert(!(G(F("p1")) /\ G(F("p2")) /\ G(F("p3")) ==> G("q" ==> F("r"))) == fairness(3))
  }

  test("nnf") {
    assert(fairness(1).nnf() == ((False R (True U "p1")) /\ (True U ("q" /\ (False R !"r")))))
    assert((!G(F("p1"))).nnf() == (True U (False R !"p1")))
    assert(((X(False) /\ X(True U X("p"))) ==> "q").nnf() == True)
    // counter example:
    // println(F(!G("p0") R (True U ("p2" <=> X("p1")))).nnf().show())
    // (True U !"p0") R (True U (("p2" /\ X("p1")) \/ (!"p2" /\ X(!"p1"))))
    // (True U ((True U !"p0") R (True U ((!"p2" \/ X("p1")) /\ (X(!"p1") \/ "p2")))))
  }
