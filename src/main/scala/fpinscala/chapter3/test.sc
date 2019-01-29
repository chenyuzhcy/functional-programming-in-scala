object test {
  import fpinscala.chapter3.List

  val list = List(1, 2, 3, 4)

  List.init(list)

  List.increase(list, 1)

  List.flatMap(list)(i => List(i, i))

  List.hasSubsequence(list, List(2,3))
}