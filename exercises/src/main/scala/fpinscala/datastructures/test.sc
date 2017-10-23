import fpinscala.datastructures._

val x = List(1,2,3,4,5,1)
val y = List("one","two","three")
List.sum(x)
List.tail(x)
List.length(x)

def reverse[A](l: List[A]): List[A] = {
  val st: List[A] = Nil
  List.foldLeft(l, List[A]())((acc, x) => Cons(x , acc))
}

reverse(x)
reverse(y)