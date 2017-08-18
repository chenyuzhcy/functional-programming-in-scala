package fpinscala.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if(n <= 0) l
    else
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n-1)
      }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  // 3.9
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  // 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => acc
      case Cons(h, t) => foldLeft(t, f(acc, h))
    }
  }

  // 3.11
  def sum2(ints: List[Int]): Int =
    foldLeft(ints, 0)((acc, h) => acc + h)

  def product2(ds: List[Double]): Double =
    foldLeft(ds, 1.0)((acc, h) => acc * h)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((acc, _) => acc + 1)

  // 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((acc, x) => Cons(x, acc))

  // 3.13

  // 3.14
  def append[A](l1: List[A], l2: List[A]): List[A] =
    foldRight(l1, l2)((x, acc) => Cons(x, acc))

  // 3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil:List[A])((acc, x) => append(acc, x))
}
