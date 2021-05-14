import Prep._
import Names._

extension (p: Prep)
  def unary_! : Prep = Not(p)
  def *(q: Prep): Prep = And(p, q)
  def +(q: Prep): Prep = Or(p, q)

given (String => Prep) = Prep.Var.apply

@main def print_expr = 
  val (a, b, c, d, e, f) = latin.bind[Prep](6)
  val g = !b * (!f + c + a) * (d + (!e * c * b)) * (e + !(c * a)) + (!e * f)

  val (fmap, rmap) = g.vars_mappings()
  val solution = dpll(g.cnf(fmap)).get
  val var_config = solution.map(i => (rmap(Math.abs(i)), i > 0)).toMap

  println(g.pretty())
  println(var_config)
  println(g.eval(var_config))
