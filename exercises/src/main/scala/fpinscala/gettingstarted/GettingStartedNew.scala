package fpinscala.gettingstarted

import scala.annotation.tailrec

object GettingStartedNew extends App {
  def fib(n: Int): Int = {

    @tailrec
    def loop(i: Int, curr: Int, next: Int): Int =
      if (i == 0) curr
      else loop(i - 1, next, curr + next)

    loop(n, 0, 1)
  }

  //  (0 to 10).foreach(i => println(fib(i)))

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def loop(i: Int): Boolean = {
      if (i >= as.length - 1) true
      else if (!ordered(as(i), as(i + 1))) false
      else loop(i + 1)
    }

    loop(0)
  }

  //  val arr1 = Array(1, 2, 3, 4, 6, 6)
  //  val arr2 = Array(1, 2, 3, 6, 5, 6)
  //  println(isSorted[Int](arr1, _ <= _))
  //  println(isSorted[Int](arr2, _ <= _))
  //  val arr3 = Array[Int]()
  //  println(isSorted[Int](arr3, _ <= _))

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
