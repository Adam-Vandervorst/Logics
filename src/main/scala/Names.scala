import scala.compiletime.ops.int.S
import scala.reflect.ClassTag


type Mul[N <: Int, T] =
  N match
    case 0 => EmptyTuple
    case S[prev] => Tuple.Concat[Mul[prev, T], Tuple1[T]]

private def toLetters(n: Int): String =
  if (n == 0) "A"
  else if (n < 0) throw RuntimeException()
  else List.unfold(n)(i =>
    if (i > 0) Some(Names.Latin.data(i % 26), i/26) else None
  ).mkString("")

enum Names(val data: Seq[String]):
  case latin extends Names("abcdefghijklmnopqrstuvwxyz".map(_.toString))
  case Latin extends Names("ABCDEFGHIJKLMNOPQRSTUVWXYZ".map(_.toString))
  case greek extends Names("αβγδεζηθικλμνξοπρστυφχψω".map(_.toString))
  case Greek extends Names("ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ".map(_.toString))
  case cyrillic extends Names("абвгдежзийклмнопрстуфхцчшщъыьэюя".map(_.toString))
  case Cyrillic extends Names("АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ".map(_.toString))
  case Hebrew extends Names("אבּבגדהוזחטיכּכךּךלמםנןסעפּפףצץקרשׁשׂתּת".map(_.toString))
  case script extends Names("\uD835\uDCEA\uD835\uDCEB\uD835\uDCEC\uD835\uDCED\uD835\uDCEE\uD835\uDCEF\uD835\uDCF0\uD835\uDCF1\uD835\uDCF2\uD835\uDCF3\uD835\uDCF4\uD835\uDCF5\uD835\uDCF6\uD835\uDCF7\uD835\uDCF8\uD835\uDCF9\uD835\uDCFA\uD835\uDCFB\uD835\uDCFC\uD835\uDCFD\uD835\uDCFE\uD835\uDCFF\uD835\uDD00\uD835\uDD01\uD835\uDD02\uD835\uDD03".map(_.toString))
  case Script extends Names("\uD835\uDCD0\uD835\uDCD1\uD835\uDCD2\uD835\uDCD3\uD835\uDCD4\uD835\uDCD5\uD835\uDCD6\uD835\uDCD7\uD835\uDCD8\uD835\uDCD9\uD835\uDCDA\uD835\uDCDB\uD835\uDCDC\uD835\uDCDD\uD835\uDCDE\uD835\uDCDF\uD835\uDCE0\uD835\uDCE1\uD835\uDCE2\uD835\uDCE3\uD835\uDCE4\uD835\uDCE5\uD835\uDCE6\uD835\uDCE7\uD835\uDCE8\uD835\uDCE9".map(_.toString))
  case fraktur extends Names("\uD835\uDD86\uD835\uDD87\uD835\uDD88\uD835\uDD89\uD835\uDD8A\uD835\uDD8B\uD835\uDD8C\uD835\uDD8D\uD835\uDD8E\uD835\uDD8F\uD835\uDD90\uD835\uDD91\uD835\uDD92\uD835\uDD93\uD835\uDD94\uD835\uDD95\uD835\uDD96\uD835\uDD97\uD835\uDD98\uD835\uDD99\uD835\uDD9A\uD835\uDD9B\uD835\uDD9C\uD835\uDD9D\uD835\uDD9E\uD835\uDD9F".map(_.toString))
  case Fraktur extends Names("\uD835\uDD6C\uD835\uDD6D\uD835\uDD6E\uD835\uDD6F\uD835\uDD70\uD835\uDD71\uD835\uDD72\uD835\uDD73\uD835\uDD74\uD835\uDD75\uD835\uDD76\uD835\uDD77\uD835\uDD78\uD835\uDD79\uD835\uDD7A\uD835\uDD7B\uD835\uDD7C\uD835\uDD7D\uD835\uDD7E\uD835\uDD7F\uD835\uDD80\uD835\uDD81\uD835\uDD82\uD835\uDD83\uD835\uDD84\uD835\uDD85".map(_.toString))
  case struck extends Names("\uD835\uDD52\uD835\uDD53\uD835\uDD54\uD835\uDD55\uD835\uDD56\uD835\uDD57\uD835\uDD58\uD835\uDD59\uD835\uDD5A\uD835\uDD5B\uD835\uDD5C\uD835\uDD5D\uD835\uDD5E\uD835\uDD5F\uD835\uDD60\uD835\uDD61\uD835\uDD62\uD835\uDD63\uD835\uDD64\uD835\uDD65\uD835\uDD66\uD835\uDD67\uD835\uDD68\uD835\uDD69\uD835\uDD6A\uD835\uDD6B".map(_.toString))
  case Struck extends Names("\uD835\uDD38\uD835\uDD39ℂ\uD835\uDD3B\uD835\uDD3C\uD835\uDD3D\uD835\uDD3Eℍ\uD835\uDD40\uD835\uDD41\uD835\uDD42\uD835\uDD43\uD835\uDD44ℕ\uD835\uDD46ℙℚℝ\uD835\uDD4A\uD835\uDD4B\uD835\uDD4C\uD835\uDD4D\uD835\uDD4E\uD835\uDD4F\uD835\uDD50ℤ".map(_.toString))

  case Numbers extends Names(LazyList.from(0).map(_.toString))
  case Letters extends Names(LazyList.unfold(0)(n => Some((toLetters(n), n + 1))))

  def bind[T : ClassTag](n: Int)(using bind_map: String => T) =
    val vars = this.data.take(n).map(bind_map)
    Tuple.fromArray(vars.toArray).asInstanceOf[Mul[n.type, T]]
