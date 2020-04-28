package fpinscala.datastructures

object ListTest extends App {
  println(List.x)

  val l = List(1, 2, 3)
  println(s"tail of $l is ${List.tail(l)}")
  println(s"replace head with 5: ${List.setHead(l, 5)}")

  val ll = List(1, 2, 3, 4, 5)
  println(s"after drop: ${List.drop(ll, 2)}")

  println(s"after drop while < 4: ${List.dropWhile(ll, (e: Int) => e < 4)}")
  println(s"init of ll list: ${List.init(ll)}")
  println(s"the length of ll: ${List.length(ll)}")

  println(s"sum of l with fold left: ${List.sumViaFoldLeft(l)}")
  println(s"product of ll with fold left: ${List.productViaFoldLeft(ll)}")
  println(s"length of l with fold left: ${List.length(l)}")

  println(s"reverse of ll : ${List.reverse(ll)}")

  val l1 = List(1, 2, 3)
  val l2 = List(4, 5, 6)
  println(s"appendViaFoldRight: ${List.appendViaFoldRight(l2, l1)}")

  val lll = List (List(1, 2), List(3, 4), List(5, 6))
  println(s"concat: ${List.concat(lll)}")

  // 3.16
  println(s"after add1: ${List.add1(l2)}")

  // 3.17
  println(s"${List.toListOfStrings(List(1.0, 2.0, 3.0))}")

  // 3.19
  println(s"filter even numbers: " +
    s"${List.filter(List(1, 2, 3, 4, 5, 6, 7, 8)) (_ % 2 != 0)}")
}