package fpinscala.chapter12

import fpinscala.chapter10.{Foldable, Monoid}
import fpinscala.chapter11.Functor

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] = sequence[G, B](map(fa)(f))
  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] = traverse(fga)(identity)

  //12.14
  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = a
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma)
  }
  def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idMonad)


  type Const[M, B] = M
  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      override def unit[A](a: => A): M = M.zero

      override def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
    }

  override def foldMap[A, M](as: F[A])(f: A => M)(M: Monoid[M]): M =
    traverse[({type f[x] = Const[M, x]})#f, A, Nothing](as)(f)(monoidApplicative(M))

}

object Traverse {
  // 12.13
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](la: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      la.foldRight(G.unit(List.empty[B]))((a, glb) => G.map2(f(a), glb)(_::_))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = oa match {
      case Some(a) => G.map(f(a))(Some(_))
      case None => G.unit(None)
    }
  }

  case class Tree[+A](head: A, tail: List[Tree[A]])
  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = {
      val headG = f(ta.head)
      val tailG = ta.tail.foldRight(G.unit(List.empty[Tree[B]]))((ta, lgt) => G.map2(traverse(ta)(f), lgt)(_::_))
      G.map2(headG, tailG)(Tree(_, _))
    }
  }


}
