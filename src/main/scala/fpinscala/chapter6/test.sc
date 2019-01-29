import fpinscala.chapter6.{Coin, Machine, Turn, candyMachine}

object test {

  var actions = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
  var simulation = candyMachine.simulateMachine(actions)
  simulation.run(Machine(true, 5, 10))
}