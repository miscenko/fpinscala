package fpinscala.laziness

import fpinscala.laziness.Stream.{cons, empty, unfold}

import scala.annotation.tailrec

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // 5.1
  def toListNonTailrec: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListNonTailrec
  }

  def toList: List[A] = {

    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }

    go(this, List()).reverse
  }

  // 5.2
  def take(n: Int): Stream[A] =
    if (n == 0) Empty
    else this match {
      case Empty => this
      case Cons(h, t) => cons(h(), t().take(n - 1))
    }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if (n > 0) => t().drop(n - 1)
    case _ => this
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  // 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // 5.5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  // 5.6
  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => true): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[AA >: A](s: => Stream[AA]): Stream[AA] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  // 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), empty))
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
      case _ => None
    }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream extends App {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val stream1 = cons(1, cons(2, cons(3, Empty)))
  //  println(stream1.toListNonTailrec)
  //  println(stream1.toList)

  //  println(stream1.take(2).toList)
  //  println(stream1.drop(1).toList)
  //println(stream1.takeWhile(_ < 2).toList)
  //  println(stream1.forAll(_ < 2))
  //  println(stream1.forAll(_ < 10))
  // println(stream1.takeWhileViaFoldRight(_ < 3).toList)
  // println(stream1.headOptionViaFoldRight)

  // 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // println(constant(42).take(10).toList)

  // 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // 5.10
  def fib: Stream[Int] = {
    def go(curr: Int, next: Int): Stream[Int] =
      cons(curr, go(next, curr + next))

    go(0, 1)
  }

  // println(fib.take(10).toList)

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  // 5.12
  def fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (curr, next) => Some((curr, (next, curr + next))) }

  println(fibsViaUnfold.take(10).toList)

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(n => Some((a, a)))

  def onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  // 5.14
  // 5.15
  // 5.16
}