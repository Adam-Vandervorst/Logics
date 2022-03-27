import scala.reflect.ClassTag

private def toLetters(n: Int): String =
  val l = Names.Latin.data(n % 26)
  if n > 26 then toLetters(n/26) + l
  else if n == 26 then "A" + l
  else l

enum Names(val data: Seq[String]):
  case latin extends Names("abcdefghijklmnopqrstuvwxyz".map(_.toString))
  case Latin extends Names("ABCDEFGHIJKLMNOPQRSTUVWXYZ".map(_.toString))
  case greek extends Names("αβγδεζηθικλμνξοπρστυφχψω".map(_.toString))
  case Greek extends Names("ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ".map(_.toString))
  case cyrillic extends Names("абвгдежзийклмнопрстуфхцчшщъыьэюя".map(_.toString))
  case Cyrillic extends Names("АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ".map(_.toString))
  case Hebrew extends Names("אבּבגדהוזחטיכּכךּךלמםנןסעפּפףצץקרשׁשׂתּת".map(_.toString))
  case script extends Names("𝓪𝓫𝓬𝓭𝓮𝓯𝓰𝓱𝓲𝓳𝓴𝓵𝓶𝓷𝓸𝓹𝓺𝓻𝓼𝓽𝓾𝓿𝔀𝔁𝔂𝔃".map(_.toString))
  case Script extends Names("𝓐𝓑𝓒𝓓𝓔𝓕𝓖𝓗𝓘𝓙𝓚𝓛𝓜𝓝𝓞𝓟𝓠𝓡𝓢𝓣𝓤𝓥𝓦𝓧𝓨𝓩".map(_.toString))
  case fraktur extends Names("𝖆𝖇𝖈𝖉𝖊𝖋𝖌𝖍𝖎𝖏𝖐𝖑𝖒𝖓𝖔𝖕𝖖𝖗𝖘𝖙𝖚𝖛𝖜𝖝𝖞𝖟".map(_.toString))
  case Fraktur extends Names("𝕬𝕭𝕮𝕯𝕰𝕱𝕲𝕳𝕴𝕵𝕶𝕷𝕸𝕹𝕺𝕻𝕼𝕽𝕾𝕿𝖀𝖁𝖂𝖃𝖄𝖅".map(_.toString))
  case struck extends Names("𝕒𝕓𝕔𝕕𝕖𝕗𝕘𝕙𝕚𝕛𝕜𝕝𝕞𝕟𝕠𝕡𝕢𝕣𝕤𝕥𝕦𝕧𝕨𝕩𝕪𝕫".map(_.toString))
  case Struck extends Names("𝔸𝔹ℂ𝔻𝔼𝔽𝔾ℍ𝕀𝕁𝕂𝕃𝕄ℕ𝕆ℙℚℝ𝕊𝕋𝕌𝕍𝕎𝕏𝕐ℤ".map(_.toString))

  case Numbers extends Names(LazyList.from(0).map(_.toString))
  case Letters extends Names(LazyList.unfold(0)(n => Some((toLetters(n), n + 1))))

  case NATO extends Names("Alfa Bravo Charlie Delta Echo Foxtrot Golf Hotel India Juliett Kilo Lima Mike November Oscar Papa Quebec Romeo Sierra Tango Uniform Victor Whiskey X-ray Yankee Zulu".split(" ").toSeq)

  def bind[T : ClassTag](n: Int)(using f: String => T): Mul[n.type, T] =
    val vars = this.data.take(n).map(f)
    Tuple.fromArray(vars.toArray).asInstanceOf[Mul[n.type, T]]
