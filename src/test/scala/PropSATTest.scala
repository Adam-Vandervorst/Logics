import scala.collection.immutable.SortedSet
import org.scalatest.funsuite.AnyFunSuite


class CNFSATTest extends AnyFunSuite:
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

  val ps6 = CNF(Set(
    Set(1, -1, 2),
    Set(2, 3),
    Set(1, 4)
  ))

  test("ps1 ps2 ps3 pureSplit") {
    assert(ps3.pureSplit == (Set(1, 2, 3), CNF.vacuous))
    assert(ps1.pureSplit == (Set(-1, -5), CNF.vacuous))
    assert(ps2.pureSplit == (Set(4), CNF(Set(Set(1, 2, 3),
      Set(-1, 2, 3),
      Set(1, -2, 3),
      Set(1, 2, -3)
    ))))
  }

  test("ps6 withoutDual") {
    println(ps6.withoutDual)
  }

  test("ps1 ps2 ps3 withoutSubsumed") {
    assert(ps1.withoutSubsumed == ps1)
    assert(ps2.withoutSubsumed == ps2)
    assert(ps3.withoutSubsumed == CNF(Set(
      Set(1, 2, 3),
      Set(1, -4)
    )))
  }

  test("ps4 ps5 dpll") {
    assert(dpll(ps4).get.contains(-2))
    assert(dpll(ps5).isEmpty)
  }
