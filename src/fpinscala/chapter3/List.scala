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
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
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

  // 3.16
  def increase(l: List[Int], num: Int): List[Int] =
    foldRight(l, Nil: List[Int])((x, acc) => Cons(x+num, acc))

  // 3.17
  def toString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((x, acc) => Cons(x.toString(), acc))

  // 3.18
  def map[A, B](l: List[A])(f: A=>B): List[B] =
    foldRight(l, Nil: List[B])((x, acc) => Cons(f(x), acc))

  // 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((x, acc) => if (f(x)) Cons(x, acc) else acc)

  // 3.20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldLeft(l, Nil: List[B])((acc, x) => append(acc, f(x)))

  def flatMap2[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  // 3.21
  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if(f(a)) List(a) else Nil)

  // 3.22
  def addTogether(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, addTogether(t1, t2))
    }
  }

  // 3.23
  def zipWith[A, B, C](la: List[A], lb: List[B])(f: (A, B)=>C): List[C] = {
    (la, lb) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
    }
  }


  // 3.25
  def startsWith[A](l1: List[A], l2: List[A]): Boolean = {
    (l1, l2) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
      case _ => false
    }
  }
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => false
      case Cons(_, _) if startsWith(sup, sub) => true
      case Cons(_, t) => hasSubsequence(t, sub)
    }
  }
}
