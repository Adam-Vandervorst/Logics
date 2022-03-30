import scala.collection.immutable.SortedSet
import org.scalatest.funsuite.AnyFunSuite
import CTL.*


class CTLTest extends AnyFunSuite:
  test("pre-image") {
    import VacuumWorld.*
    assert(vacuumWorld.ex_preimg(Set(s4)) == Set(s4, s7))
    assert(vacuumWorld.ex_preimg(Set(s6, s8)) == Set(s6, s8, s5, s3))
    assert(vacuumWorld.ex_preimg(Set(s1, s7, s5, s6)) == vacuumWorld.states)
  }

  test("vacuum CTL") {
    import VacuumWorld.*
    assert(CTLCheck(vacuumWorld, Var("inA")) == Set(s1, s3, s4, s5))                             //inA
    assert(CTLCheck(vacuumWorld, Not(Var("inA"))) == Set(s2, s6, s7, s8))                        //¬inA
    assert(CTLCheck(vacuumWorld, And(Not(Var("inA")), Var("inB"))) == Set(s2, s6, s7, s8))
    assert(CTLCheck(vacuumWorld, And(Not(Var("cleanA")), Not(Var("cleanB")))) == Set(s1, s2))    //¬cleanA ∧ ¬cleanB
    assert(CTLCheck(vacuumWorld, EX(And(Var("cleanA"), Var("cleanB")))) == Set(s4, s5, s6, s8))  //EX (cleanA ∧ cleanB)
    assert(CTLCheck(vacuumWorld, And(EU(Var("inA"), Var("cleanA")), Not(Var("cleanB")))) == Set(s1, s3, s6))
  }

  test("rocket CTL"){
    import RocketWorld.*
    assert(CTLCheck(rocketWorld, EF(Var("caR"))) == rocketWorld.states.toSet)
    assert(CTLCheck(rocketWorld, AG(Or(Var("roL"), Var("caL")))) == Set())
    assert(CTLCheck(rocketWorld, EU(Var("fuelOK"), Var("caR"))) == Set(s2, s5, s6, s7, s8, s12))
  }

