object test{
  import fpinscala.chapter2.Exercise

  var fibNumber = Exercise.fib(6)

  var sorted = Exercise.isSorted(Array(1,3,2),
    (num1: Int, num2: Int) => num1<num2)
}