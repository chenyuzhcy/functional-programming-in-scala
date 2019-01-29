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
  def optionMoniod[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    override def zero: Option[A] = None
  }

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

  // 10.16
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      def op(x: (A, B), y: (A, B)): (A, B) = (A.op(x._1, y._1), B.op(x._2, y._2))
      def zero: (A, B) = (A.zero, B.zero)
    }

  // 10.17
  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[(A) => B] {
      override def op(x: (A) => B, y: (A) => B): (A) => B =
        a => B.op(x(a), y(a))
      override def zero: (A) => B =
        _ => B.zero
    }

  // 10.18

}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WCMonoid {
  import Monoid._

  // 10.10
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

  // 10.11
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
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
  // 10.15
  def toList[A](fa: F[A]): List[A] =
    foldRight(fa)(List[A]())((a, b) => a::b)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Foldable {
  // 10.12
  val foldableList = new Foldable[List] {
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  }

  // 10.13
  val foldableTree = new Foldable[Tree] {
    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Leaf(a) => f(a, z)
        case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
      }

    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(a) => f(z, a)
        case Branch(l, r) => foldLeft(l)(foldLeft(r)(z)(f))(f)
      }
  }

  // 10.14
  val foldableOption = new Foldable[Option] {
    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
  }

}
