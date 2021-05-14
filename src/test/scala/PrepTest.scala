import scala.collection.immutable.SortedSet
import org.scalatest.funsuite.AnyFunSuite
import Prep._


class PrepTest extends AnyFunSuite:
  val e1 = And(Or(Var("x"), False), Not(Var("y")))

  test("simple expr eval") {
    assert(e1.eval(Map("x" -> true, "y" -> false)) == true)
    assert(e1.eval(Map("x" -> true, "y" -> true)) == false)
  }

  test("'simple expr vars") {
    assert(e1.vars() == SortedSet("x", "y"))
  }

  test("'simple expr vars_mapping") {
    val (fmap, rmap) = e1.vars_mappings()
    assert(fmap == Map("x" -> 1, "y" -> 2))
    assert(rmap == Map(1 -> "x", 2 -> "y"))
  }

  test("simple expr to_dot") (pending)

  test("simple expr simplify") {
    assert(e1.simplify() == And(Var("x"), Not(Var("y"))))
  }

  test("simple expr forrest") {
    assert(e1.forrest[And]() == Set(Or(Var("x"), False), Not(Var("y"))))
  }

  test("simple expr cnf") {
    assert(e1.cnf(e1.vars_mappings()._1) == Set(Set(1), Set(-2)))
  }
