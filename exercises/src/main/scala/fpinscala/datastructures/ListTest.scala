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
}
