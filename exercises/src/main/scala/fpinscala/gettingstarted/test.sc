import scala.annotation.tailrec

object test {
  def fib(n: Int): Int = {
    @tailrec
    def loop (n: Int, prev1: Int, prev2: Int): Int = {
      /* // my answer
      if (i < 2) i
      else if (i == 2) prev1 + prev2
      else loop(i-1, prev2, prev1 + prev2)
      */

      // real answer is better
      if (n == 0) prev1
      else loop(n - 1, prev2, prev1 + prev2)

    }

    loop(n, 0, 1)
  }

  fib(0)
  fib(1)
  fib(2)
  fib(3)
  fib(4)
  fib(5)
  fib(6)
}