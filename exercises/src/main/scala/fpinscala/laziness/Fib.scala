package fpinscala.laziness

object Fib extends App {

  def fib: LazyList[Int] = {
    def go(curr: Int, next: Int): LazyList[Int] =
      curr #:: go(next, curr + next)

    go(0, 1)
  }

  println(fib.take(20).toList)
}
