package fpinscala.chapter2

object Exercise {

  // 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def rec(n: Int, prev: Int, cur: Int): Int =
      if(n==0) prev
      else rec(n-1, cur, prev + cur)

    rec(n, 0, 1)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n>=as.length) true
      else if (ordered(as(n-1), as(n))) loop(n+1)
      else false

    loop(1)
  }

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a) => (b) => f(a, b)
  }

  // 2.4
  def uncurry[A, B, C](f: A =>B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a) => f(g(a))
  }
}
