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

  //////////////////////////////////////////////////////////

  def startsWith[B](s: Stream[B]): Boolean = ???

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = ???

  //different from book - didn't test it
  def tails: Stream[Stream[A]] = {
    def tailsLoop(s: Stream[A]): Stream[Stream[A]] =
      unfold(s) {
      case Cons(h, t) => Some((s, t()))
      case _ => None
    }

    tailsLoop(this) append Stream(empty)
  }

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

  def fib: Stream[Int] = {
    def fibLoop(pred1: Int, pred2: Int): Stream[Int] =
      cons(pred1, fibLoop(pred2, pred1 + pred2))

    fibLoop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons (h, unfold(s)(f))
    case None => empty
  }

  def fibWithUnfold: Stream[Int] =
    unfold((0,1)) { case (p1, p2) => Some((p1, (p2, p1 + p2))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n + 1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  val onesViaUnfold = unfold(1)(_ => Some(1,1))

}