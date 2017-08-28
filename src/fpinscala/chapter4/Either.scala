package fpinscala.chapter4

sealed trait Either[+E, +A]{

  // 4.6
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    (this, b) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Left(ea), _) => Left(ea)
      case (_, Left(eb)) => Left(eb)
    }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  // 4.7
  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    as match {
      case Nil => Right(Nil)
      case h::t => h.map2(sequence(t))((a, b) => a::b)
    }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case h::t => f(h).map2(traverse(t)(f))((a, b) => a::b)
    }
}
