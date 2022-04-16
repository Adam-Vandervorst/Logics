import scala.collection.immutable.SortedSet
import org.scalatest.funsuite.AnyFunSuite


class BATest extends AnyFunSuite:
  val ba1: BA[String] = BA()
  val ba1_nodes: Seq[ba1.State] = {
    val n = ba1.init +: Seq.tabulate(3)(_ => ba1.newNode())
    ba1.connect(n(0), n(1), Set("i1"))
    ba1.connect(n(0), n(2), Set("i2"))
    ba1.connect(n(1), n(3), Set("f1"))
    ba1.connect(n(2), n(3), Set("f2"))
    n
  }

  test("creation") {
    pending
  }

