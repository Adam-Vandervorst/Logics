import scala.collection.immutable.SortedSet
import org.scalatest.funsuite.AnyFunSuite
import Prop.*


class PropTest extends AnyFunSuite:
  val e1 = And(Or(Var("x"), False), Not(Var("y")))

  test("e1 positiev negative eval") {
    assert(e1.eval(Map("x" -> true, "y" -> false)))
    assert(!e1.eval(Map("x" -> true, "y" -> true)))
  }

  test("e1 expr vars") {
    assert(e1.vars() == SortedSet("x", "y"))
  }

  test("e1 expr vars_mapping") {
    val (fmap, rmap) = e1.vars_mappings()
    assert(fmap == Map("x" -> 1, "y" -> 2))
    assert(rmap == Map(1 -> "x", 2 -> "y"))
  }

  test("e1 expr pretty") (pending)
  
  test("e1 expr to_dot") (pending)

  test("e1 expr simplify") {
    assert(e1.simplify() == And(Var("x"), Not(Var("y"))))
  }

  test("e1 expr forrest") {
    assert(e1.forrest[And]() == Set(Or(Var("x"), False), Not(Var("y"))))
  }

  test("e1 expr cnf") {
    assert(e1.cnf(e1.vars_mappings()._1) == CNF(Set(Set(1), Set(-2))))
  }

class DNFTest extends AnyFunSuite:
  val ps1 = DNF(Set(
    Set(1, 2),
    Set(1, -2),
    Set(3, -4),
    Set(3, 4)
  ))

  test("ps1 withoutImpure") {
    assert(ps1.withoutImpure == DNF(Set(Set(1), Set(3))))
  }