package fpinscala.chapter10

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  // 10.1
  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  // 10.2

  // 10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A=>A, a2: A=>A): A => A = (a: A) => a2(a1(a))
    def zero: A=>A = (a: A) => a
  }

  // 10.4

  // 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  // 10.6

  // 10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val vLen = v.length
    if (vLen == 0) m.zero
    else if (vLen == 1) f(v(0))
    else {
      val (x, y) = v.splitAt(vLen/2)
      m.op(foldMapV(x, m)(f), foldMapV(y, m)(f))
    }
  }

  // 10.8

  // 10.9

}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WCMonoid {
  import Monoid._

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = {
      (a1, a2) match {
        case (Stub(a), Stub(b)) => Stub(a + b)
        case (Stub(a), Part(l, w, r)) => Part(a + l, w, r)
        case (Part(l, w, r), Stub(b)) => Part(l, w, r + b)
        case (Part(al, aw, ar), Part(bl, bw, br)) => {
          val c = if ((ar + bl).isEmpty) 0 else 1
          Part(al, aw + bw + c, br)
        }
      }
    }
    def zero: WC = Stub("")
  }

  def countWords(s: String): Int = {
    def toWC(c: Char): WC =
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString())

    def countStub(a: String): Int =
      if (a.isEmpty) 0 else 1

    val wc = foldMapV(s.toIndexedSeq, wcMonoid)(toWC)
    wc match {
      case Stub(a) => countStub(a)
      case Part(l, w, r) => countStub(l) + w + countStub(r)
    }
  }
}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
}
