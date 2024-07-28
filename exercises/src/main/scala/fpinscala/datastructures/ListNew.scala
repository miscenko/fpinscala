package fpinscala.datastructures

import fpinscala.datastructures.ListNew.List.{addOne, addPairs, appendViaFoldLeft, appendViaFoldRight, convertDoubleToString, drop, dropWhile, filter, flatMap, init, length, length2, product, product2, reverse, sum, sum2, tail}

import scala.annotation.tailrec

object ListNew extends App {
  sealed trait List[+A] // `List` data type, parameterized on a type, `A`

  case object Nil extends List[Nothing] // A `List` data constructor representing the empty list

  /* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
  which may be `Nil` or another `Cons`.
   */
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List { // `List` companion object. Contains functions for creating and working with lists.
    def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = // Variadic function syntax
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A](list: List[A]): List[A] = list match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, xs) => xs
    }

    def setHead[A](h: A, list: List[A]): List[A] = Cons(h, list)

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] =
      if (n <= 0) l
      else l match {
        case Nil => sys.error("drop on empty list")
        case Cons(_, t) => drop(t, n - 1)
      }

    @tailrec
    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t)(f) else l
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => 1 + acc)

    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

    def sum2(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

    def product2(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

    def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

    def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, a) => Cons(a, acc))

    def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] =
      foldLeft(reverse(l), r)((acc, a) => Cons(a, acc))

    def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
      foldRight(l, r)((a, acc) => Cons(a, acc))

    def concatenate[A](ll: List[List[A]]): List[A] =
      foldRight(ll, List[A]())(appendViaFoldRight)

    def addOne(l: List[Int]): List[Int] =
      foldRight(l, List[Int]())((a, acc) => Cons(a + 1, acc))

    // 3.17
    def convertDoubleToString(l: List[Double]): List[String] =
      foldRight(l, List[String]())((a, acc) => Cons(a.toString, acc))

    // 3.18
    def map[A, B](as: List[A])(f: A => B): List[B] =
      foldRight(as, List[B]())((a, acc) => Cons(f(a), acc))

    // 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, List[A]())((a, acc) => if (f(a)) Cons(a, acc) else acc)

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      foldRight(as, List[B]())((a, acc) => appendViaFoldRight(f(a), acc))

    // 3.21
    def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
      flatMap(l)(a => if (f(a)) List(a) else Nil)

    // 3.22
    def addPairs(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
      case (Nil, Nil) | (_, Nil) | (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairs(t1, t2))
    }

    def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = (l, r) match {
      case (Nil, Nil) | (_, Nil) | (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

    // 3.24

  }

  //  val x: Int = List(1, 2, 3, 4, 5) match {
  //    case Cons(x, Cons(2, Cons(4, _))) => x
  //    case Nil => 42
  //    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  //    case Cons(h, t) => h + sum(t)
  //    case _ => 101
  //  }
  //
  //  println(s"x = $x")
  //
  val l = List(1, 2, 3, 4, 6, 7)
  val l2 = List(10, 22, 32, 43, 64)
  val ld = List(1.1, 2.2, 3.3, 4.4, 6.6)
  //  println(l)
  //  println(sum(l))
  //  println(product(List(2.2, 3.3)))
  //  println(tail(l))
  //  println(tail(Nil))

  // println(drop(l, 2))
  // println(dropWhile(l) (_ < 4))
  // println(init(l))

  // println(length(l))
  // println(sum2(l))
  // println(product2(ld))
  // println(length2(l))
  //  println(reverse(l))
  //
  //  println(appendViaFoldLeft(l, l2))
  //  println(appendViaFoldRight(l, l2))

  // println(addOne(l))
  //println(convertDoubleToString(ld))
  // println(filter(l)(_ % 2 == 0))
  // println(flatMap(l)(a => List(a, a)))
  //  println(addPairs(l, l2))
  //  println(addPairs(l, Nil))
  //  println(addPairs(Nil, l2))

}