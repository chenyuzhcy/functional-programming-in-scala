package fpinscala.chapter11

import fpinscala.chapter6._

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(x => x._1), map(fab)(x => x._2))
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa) => map(fa)(a => Left(a))
      case Right(fb) => map(fb)(b => Right(b))
    }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // 11.3
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((fa, fla) => map2(fa, fla)(_::_))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, flb) => map2(f(a), flb)(_::_))

  // 11.4
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  // 11.5
  // replicateM: replicate monad n times and combine them in one monad

  // 11.6
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ???

  // 11.7
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    (a: A) => flatMap(f(a))(g)

  // 11.8
  def flatMap_compose[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

  // 11.9
  // prove

  // 11.10
  // prove

  // 11.11
  // prove

  // 11.12
  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(identity)

  // 11.13
  def flatMap_join[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  def compose_join[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    (a: A) => join(map(f(a))(g))

  // 11.14
  // monad laws in join, map and unit

  // 11.15
  // associative law for Par and Parser

  // 11.16
  // identity lars for Gen and List
}

case class Id[A](value: A)

object Monad {
  // 11.1
    // Par

    // Parser

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A): Stream[A] = Stream(a)
    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }

  // 11.17
  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma.value)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st.flatMap(f)
  }

}
