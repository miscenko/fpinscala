import fpinscala.datastructures.List.foldLeft
import fpinscala.datastructures._

val x = List(1,2,3,4,5,1)
val y = List("one","two","three")
List.sum(x)
List.tail(x)
List.length(x)

def reverse[A](l: List[A]): List[A] =
  List.foldLeft(l, List[A]())((acc, x) => Cons(x , acc))

reverse(x)
reverse(y)

def append[A](a1: List[A], a2: List[A]): List[A] = {
  List.foldRight(a1, a2)((x, acc) => Cons(x, acc))
}

append(x,y)


