import scala.annotation.tailrec
import scala.compiletime.ops.int.S
import scala.reflect.ClassTag


type Mul[N <: Int, T] =
  N match
    case 0 => EmptyTuple
    case S[prev] => Tuple.Concat[Mul[prev, T], Tuple1[T]]

@tailrec
def fix[A](f: A => A)(v: A, prev: Option[A] = None): A =
  if (prev.isEmpty || v != prev.get) fix(f)(f(v), Some(v))
  else v

@tailrec
def last[A](f: A => Option[A])(v: A): A = f(v) match
  case Some(n) => last(f)(n)
  case None => v

extension[A](a: Set[A])
  def <=(b: Set[A]): Boolean = a.forall(b.contains)
  def <(b: Set[A]): Boolean = a.size < b.size && a <= b
