package fpinscala.chapter6

import State._
case class State[S, +A](run: S => (A, S)) {
  // 6.10
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

  def get[A]: State[S, S] = State(s => (s, s))
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object candyMachine {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def calNewState(s: Machine, input: Input): Machine = {
      (input, s) match {
        case (_, Machine(_, candies, _)) if candies <= 0 => s
        case (Turn, Machine(true, _, _)) => s
        case (Coin, Machine(false, _, _)) => s
        case (Coin, Machine(true, candies, coins)) if candies > 0 => Machine(false, candies, coins + 1)
        case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
      }
    }

    State((s: Machine) => {
      val newState: Machine = inputs.foldLeft(s)(calNewState)
      ((newState.candies, newState.coins), newState)
    })
  }

}