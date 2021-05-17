import scala.collection.immutable.SortedSet
import org.scalatest.funsuite.AnyFunSuite
import LTL._


class LTLTest extends AnyFunSuite:
  val p1 = List(Set("a", "b"), Set("b"), Set("b", "c"), Set("b", "c"))
  val e1 = G(Var("b"))
  val e2 = F(G(Var("c")))
  val e3 = F(And(Not(G(Var("a"))), X(Var("c"))))
  val e4 = G(Or(Var("a"), Var("c")))
  val e5 = X(Var("a"))
  val e6 = R(Var("a"), Var("c"))

  test("p1 e1 e2 e3 positive eval") {
    assert(e1.eval(p1))
    assert(e2.eval(p1))
    assert(e3.eval(p1))
  }

  test("p1 e4 e5 e6 negative eval") {
    assert(!e4.eval(p1))
    assert(!e5.eval(p1))
    assert(!e6.eval(p1))
  }
