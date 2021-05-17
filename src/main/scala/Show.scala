enum SType:
  case Algebra, Formal, Native, Graphviz

trait Show(algebra: String, formal: String, native: String, graphviz: String):
  def string(using stype: SType): String = stype match
    case SType.Algebra => algebra
    case SType.Formal => formal
    case SType.Native => native
    case SType.Graphviz => graphviz
