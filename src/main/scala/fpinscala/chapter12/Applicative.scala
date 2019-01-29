package fpinscala.chapter12

import fpinscala.chapter11.{Functor}

trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)
  def unit[A](a: => A): F[A]

  // derived
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, flb) => map2(f(a), flb)(_::_))

  // 12.1
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((a, b) => (a, b))

  // 12.2
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) => f(a))

  // 12.3
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val fbc = apply(map(fa)(f.curried))(fb)
    apply(fbc)(fc)
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    val fbcd = apply(map(fa)(f.curried))(fb)
    val fcd = apply(fbcd)(fc)
    apply(fcd)(fd)
  }

  // 12.4

  // 12.8
  def product[G[_]](G: Applicative[G]) = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))
      override def apply[A, B](ab: (F[A => B], G[A => B]))(a: (F[A], G[A])) = (self.apply(ab._1)(a._1), G.apply(ab._2)(a._2))
    }
  }

  // 12.9
  def compose[G[_]](G: Applicative[G]) = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A) = self.unit(G.unit(a))
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] =
        self.map2(fga, fgb)((ga, gb) => G.map2(ga, gb)(f))
    }
  }

  // 12.12
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(unit(Map.empty[K, V])){
      case (acc, (k, fv)) => map2(acc, fv)((m, v) => m + (k -> v))
    }
}

object Applicative {
  // 12.5
  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f]{
    def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
  }

  sealed trait Validation[+E, +A]
  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

  // 12.6
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] = new Applicative[({type f[x] = Validation[E,x]})#f] {
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
      case (Failure(ahead, atail), Failure(bhead, btail)) => Failure(ahead, atail ++ Vector(bhead) ++ btail)
      case (af@Failure(_, _), _) => af
      case (_, bf@Failure(_,_)) => bf
      case (Success(a), Success(b)) => Success(f(a, b))
    }
    override def unit[A](a: => A): Validation[E, A] = Success(a)
  }
}

trait Monad[F[_]] extends Applicative[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
}

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = sequence(map(fa)(f))
  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] = traverse(fga)(identity)

  // 12.14
  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = a
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma)
  }
  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idMonad)
}
