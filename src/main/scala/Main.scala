import LTL._
import Names._

given (String => LTL) = LTL.Var.apply

@main def print_expr = 
  val (rain, clouded, sunny) = (Var("rain"), Var("clouded"), Var("sunny"))

  val progression = List(Set("rain", "clouded"), Set("rain", "clouded"), Set("rain", "sunny"), Set("sunny"))
  val has_rainbow = F(And(rain, sunny))
  val always_clouded = G(clouded)

  println(has_rainbow.eval(progression))
  println(always_clouded.eval(progression))
