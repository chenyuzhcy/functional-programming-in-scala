package fpinscala.chapter5

import Stream._
sealed trait Stream[+A] {

  // 5.1
  def toList: List[A] = {
    @annotation.tailrec
    def loop(acc: List[A], s: Stream[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => loop(h()::acc, t())
    }

    loop(List(), this).reverse
  }

  // 5.2
  def take(n: Int): Stream[A] =
    if (n>0) {
      this match {
        case Cons(h, t) => Cons(h, () => t().take(n-1))
        case _ => Empty
      }
    }
    else Empty

  def drop(n: Int): Stream[A] =
    if (n>0) {
      this match {
        case Cons(_, t) => t().drop(n-1)
        case _ => this
      }
    }
    else this

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _ => Empty
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, acc) => p(a) && acc)

  // 5.5
  def takeWhile_2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) => if(p(a)) Cons(() => a, () => acc) else Empty)

  // 5.6
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def headOption_2: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, acc) => Cons(() => f(a), () => acc))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) => if(f(a)) Cons(() => a, () => acc) else acc)

  def append[B>:A](b: => Stream[B]): Stream[B] =
    foldRight(b)((a, acc) => Cons(() => a, () => acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, acc) => f(a).append(acc))


  // 5.13
  // use unfold to implement map, take, takeWhile, zipWith, zipAll
  def map_unfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  def take_unfold(n: Int): Stream[A] =
    unfold((this, n))({
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n-1)))
      case _ => None
    })

  def takeWhile_unfold(f: A => Boolean): Stream[A] =
    unfold(this)({
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    })

  def zipWith[B, C](b: Stream[B])(f: (A, B)=>C): Stream[C] =
    unfold((this, b))({
      case (Cons(ha, ta), Cons(hb, tb)) => Some((f(ha(), hb()), (ta(), tb())))
      case _ => None
    })

  // def zipAll[B](b: Stream[B]): Stream[(Option[A],Option[B])] =


  // 5.14
  def startsWith[A](s: Stream[A]): Boolean = {
    (this, s) match {
      case (_, Empty) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1() startsWith(t2())
      case _ => false
    }
  }

  // 5.15
  // tails(Stream[1, 2, 3]) = Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream())
  // def tails: Stream[Stream[A]]=

  // Now implement hasSubsequence in Chapter3 using stream
  // def hasSubsequence[A](s: Stream[A]): Boolean =


  // 5.16
  // generalize tails, like foldRight that returns a stream of intermediate results
  // Stream(1,2,3).scanRight(0)(_+_) => (6,5,3,0)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  // 5.8
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  // This is more efficient since it's just one object referencing itself.
  def constant_better[A](a: A): Stream[A] = {
    lazy val s: Stream[A] = Cons(() => a, () => s)
    s
  }

  // 5.9
  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  // 5.10
  def fibs: Stream[Int] = {
    def loop(n1: Int, n2: Int): Stream[Int] =
      cons(n1, loop(n2, n1+n2))

    loop(0, 1)
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty[A]
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  // 5.12
  def fibs_unfold: Stream[Int] =
    unfold((0, 1))(s => s match {
      case (n1, n2) => Some(n1, (n1, n1+n2))
    })

  def from_unfold(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s+1)))

  def constant_unfold[A](a: A): Stream[A] =
    unfold(a)(s => Some((s, s)))

  def ones_unfold: Stream[Int] =
    unfold(1)(_ => Some((1, 1)))


}
