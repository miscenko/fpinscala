package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait Stream[+A] {

  // 5.1
  def toListRecursive: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRecursive
  }

  def toList: List[A] = {
    @tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => loop(t(), h() :: acc)
      case _ => acc
    }

    loop(this, Nil).reverse
  }

  def toListFast: List[A] = {
    val buf = new ListBuffer[A]

    @tailrec
    def loop(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        loop(t())
      case _ => buf.toList
    }

    loop(this)
  }

  // 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h())  => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =   // The arrow `=>` in front of the argument type `B` means
                                                      // that the function `f` takes its second argument by name
                                                      // and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))  // If `f` doesn't evaluate its second argument, the
                                                      // recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail
                                          // of the stream. If `p(a)` returns `true`, `b` will never be evaluated
                                          // and the computation terminates early.

  // 5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  // 5.5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  // 5.6 (hard)
  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7
  // Implement map, filter, append, and flatMap using foldRight.
  // The append method should be non-strict in its argument.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // 5.13 Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold(this, n) {
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
      case _ => None
    }

  // 5.14
  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll { case (e1, e2) => e1 == e2 }

  // 5.15
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    }.append(Stream(Empty))

  def hasSubsequence[B](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  // 5.16
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = ???
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

  // 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  // 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // 5.10
  def fibs: Stream[Int] = {
    def go (f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))

    go(0, 1)
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  // 5.12
  def fibWithUnfold: Stream[Int] =
    unfold((0, 1)) { case(f0, f1) => Some((f0, (f1, f0 + f1))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some(n, n + 1))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))

}