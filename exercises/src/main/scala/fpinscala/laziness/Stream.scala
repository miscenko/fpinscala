package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // will stack overflow for large streams
  def toListRec: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRec
    case _ => List()

  }

  def toList: List[A] = {
    @annotation.tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => loop(t(), h() :: acc)
    }

    loop(this, List()).reverse
  }

  //this is incorrect (returns reversed stream)
  def take2(n: Int): Stream[A] = {
    def loop(s: Stream[A], n: Int, acc: Stream[A]): Stream[A] = {
      if (n == 0) acc
      else s match {
        case Cons(h, t) => loop(t(), n - 1, cons(h(), acc))
        case _ => acc
      }
    }

    loop(this, n, Stream())
  }

  def take3(n: Int): Stream[A] = {
    def loop (s: Stream[A], n: Int): Stream[A] = {
      if (n == 0) Stream()
      else s match {
        case Cons(h, t) => cons(h(), loop(t(), n - 1))
        case _ => Stream()
      }
    }
    loop(this, n)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    // case Cons(_, t) if n == 1 => t() //not needed
    case _ => this
  }

  def takeWhile2(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B>:A](x: => Stream[B]): Stream[B] =
    foldRight(x)((h, t) => cons(h, t))

  def flatmap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from2(n: Int): Stream[Int] = {
    lazy val tail: Stream[Int] = Cons(() => n, () => from2(n + 1 ))
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def fib(n: Int): Stream[Int] = {
    def fibLoop(pred1: Int, pred2: Int): Stream[Int] =
      cons(pred1, fibLoop(pred2, pred1 + pred2))

    fibLoop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???

}