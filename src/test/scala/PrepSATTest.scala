import scala.collection.immutable.SortedSet
import org.scalatest.funsuite.AnyFunSuite


class PrepSATTest extends AnyFunSuite:
  val ps1 = Set(
    Set(-1, -2),
    Set(-1, 2, -3),
    Set(3, -4, -5),
    Set(3, 4, -5)
  )

  val ps2 = Set(
    Set(-1, 4),
    Set(1, 2, 4),
    Set(1, 2, 3),
    Set(-1, 2, 3),
    Set(1, -2, 3),
    Set(1, 2, -3)
  )

  val ps3 = Set(
    Set(1, 2, 3),
    Set(1, 2, 3, 4),
    Set(1, -4)
  )

  val ps4 = Set(
    Set(1, -2),
    Set(-1, -2)
  )

  val ps5 = Set(
    Set(1),
    Set(-1, 2),
    Set(-2)
  )

  test("ps1 ps2 ps3 clauses remove_pure") {
    assert(remove_pure(ps3) == Set())
    assert(remove_pure(ps1) == Set())
    assert(remove_pure(ps2) == Set(Set(1, 2, 3),
      Set(-1, 2, 3),
      Set(1, -2, 3),
      Set(1, 2, -3)
    ))
  }

  test("ps1 ps2 ps3 clauses remove_subsumed") {
    assert(remove_subsumed(ps1) == ps1)
    assert(remove_subsumed(ps2) == ps2)
    assert(remove_subsumed(ps3) == Set(
      Set(1, 2, 3),
      Set(1, -4)
    ))
  }

  test("ps4 ps5 clauses dpll") {
    assert(dpll(ps4).get.contains(-2))
    assert(dpll(ps5).isEmpty)
  }
