import scala.collection.immutable.SortedSet
import org.scalatest.funsuite.AnyFunSuite


class PropSATTest extends AnyFunSuite:
  val ps1 = CNF(Set(
    Set(-1, -2),
    Set(-1, 2, -3),
    Set(3, -4, -5),
    Set(3, 4, -5)
  ))

  val ps2 = CNF(Set(
    Set(-1, 4),
    Set(1, 2, 4),
    Set(1, 2, 3),
    Set(-1, 2, 3),
    Set(1, -2, 3),
    Set(1, 2, -3)
  ))

  val ps3 = CNF(Set(
    Set(1, 2, 3),
    Set(1, 2, 3, 4),
    Set(1, -4)
  ))

  val ps4 = CNF(Set(
    Set(1, -2),
    Set(-1, -2)
  ))

  val ps5 = CNF(Set(
    Set(1),
    Set(-1, 2),
    Set(-2)
  ))

  test("ps1 ps2 ps3 clauses remove_pure") {
    assert(ps3.withoutPure == CNF.vacuous)
    assert(ps1.withoutPure == CNF.vacuous)
    assert(ps2.withoutPure == CNF(Set(Set(1, 2, 3),
      Set(-1, 2, 3),
      Set(1, -2, 3),
      Set(1, 2, -3)
    )))
  }

  test("ps1 ps2 ps3 clauses remove_subsumed") {
    assert(ps1.withoutSubsumed == ps1)
    assert(ps2.withoutSubsumed == ps2)
    assert(ps3.withoutSubsumed == CNF(Set(
      Set(1, 2, 3),
      Set(1, -4)
    )))
  }

  test("ps4 ps5 clauses dpll") {
    assert(dpll(ps4).get.contains(-2))
    assert(dpll(ps5).isEmpty)
  }
