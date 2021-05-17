import Names._

import scala.reflect.ClassTag

extension (p: AExpr[Prep])
  def unary_! : AOp[Prep] = Not(p)
  def *(q: AExpr[Prep]): AOp[Prep] = And(p, q)
  def +(q: AExpr[Prep]): AOp[Prep] = Or(p, q)

given (String => AVar[Prep]) = Var.apply


@main def print_expr =
  val (a, b, c, d, e, f) = latin.bind[AExpr[Prep]](6)
  val g = !b * (!f + c + a) * (d + (!e * c * b)) * (e + !(c * a))

  val (fmap, rmap) = g.vars_mappings()
  val solution = dpll(g.cnf(fmap)).get
  val var_config = solution.map(i => (rmap(Math.abs(i)), i > 0)).toMap

  given SType = SType.Native
  println(g.forrest[And]().map(testt))
  println(var_config)
  println(g.eval(var_config))
