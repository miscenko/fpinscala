import scala.annotation.tailrec

object test {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean =  {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n + 1 >= as.length) true
      else if (ordered(as(n), as(n+1))) loop(n+1)
      else false
    }
    loop(0)
  }

  def gt (a: Int, b: Int): Boolean = {
    a <= b
  }

  val intAr: Array[Int] = Array(0, 7, 89)

  isSorted(intAr, gt)

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

}