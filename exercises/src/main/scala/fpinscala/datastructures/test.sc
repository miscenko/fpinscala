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

val bigList = List(List("aa", "bb", "cc"), List("dd", "ee"), List("ff","gg","hh"))

def flat[A] (l: List[List[A]]) : List[A] = l match {
  case Nil => Nil
  case Cons(x, xs) => append(x, flat(xs))
}

flat(bigList)

def flat2[A] (l: List[List[A]]) : List[A] =
  List.foldRight(l, Nil:List[A])(append)

flat2(bigList)

//Exercise 3.16
def incrList(l: List[Int]): List[Int] =
  List.foldRight(l, Nil:List[Int])((el, acc) => Cons(el + 1, acc))

incrList(x)

//Exercise 3.17
def doubleToStr (l: List[Double]): List[String] =
  List.foldRight(l, Nil:List[String])((el, acc) => Cons(el.toString, acc))

val dd: List[Double] = List(2.0, 7.5, 5.4)
doubleToStr(dd)

//Exercise 3.18
List.map[Double, String](dd)(_.toString)
List.map(x)(_ + 2)