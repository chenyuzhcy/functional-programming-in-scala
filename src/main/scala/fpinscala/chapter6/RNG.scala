package fpinscala.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (integer, r) = rng.nextInt
    if (integer<0) (-(integer+1), r)
    else (integer, r)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (integer, r) = rng.nextInt
    (integer / (Int.MaxValue.toDouble + 1), r)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i,d), r2)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ((d, i), r2)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0) (List(), rng)
    else {
      val (i, r1) = rng.nextInt
      val (list, r2) = ints(count-1)(r1)
      (i::list, r2)
    }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A=>B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // 6.5
  def double_map: Rand[Double] =
    map(rng => rng.nextInt)(i => i / (Int.MaxValue.toDouble + 1))

  // 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs match {
        case Nil => (List[A](), rng)
        case h::t => {
          val (a, r1) = h(rng)
          val (l, r2) = sequence(t)(r1)
          (a::l, r2)
        }
      }
    }

  // 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i+(n-1)-mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    })

  // 6.9
  def map_flatMap[A, B](s: Rand[A])(f: A=>B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2_flatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))
}