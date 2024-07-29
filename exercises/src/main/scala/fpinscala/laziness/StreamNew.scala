package fpinscala.laziness

trait Stream[+A] {
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object StreamNew {
}