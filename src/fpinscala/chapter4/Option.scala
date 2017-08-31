package fpinscala.chapter4

sealed trait Option[+A] {
  // 4.1
  def map[B](f: A => B): Option[B] =
    this match {
      case Some(a) => Some(f(a))
      case None => None
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case Some(a) => a
      case None => default
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def flatMap2[B](f: A => Option[B]): Option[B] =
    this match {
      case Some(a) => f(a)
      case None => None
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this.map(a => Some(a)).getOrElse(ob)

  def orElse2[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case Some(a) => Some(a)
      case None => ob
    }

  def filter(f: A => Boolean): Option[A] =
    this.flatMap(a => {
      if(f(a)) Some(a)
      else None
    })

  def filter2(f: A => Boolean): Option[A] =
    this match {
      case Some(a) if f(a) => Some(a)
      case _ => None
    }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x-m, 2))))

  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _ => None
    }

  // *4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(h, sequence(t))((a, b) => a::b)
    }

  // *4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h::t => map2(f(h), traverse(t)(f))((a, b) => a::b)
    }
}
