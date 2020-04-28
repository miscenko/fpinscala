package fpinscala.datastructures

object ListTest extends App {
  println(List.x)

  val l = List(1, 2, 3)
  println(s"tail of $l is ${List.tail(l)}")
  println(s"replace head with 5: ${List.setHead(l, 5)}")

  val ll = List(1, 2, 3, 4, 5)
  println(s"after drop: ${List.drop(ll, 2)}")

  println(s"after drop while < 4: ${List.dropWhile(ll, (e: Int) => e < 4)}")
  println(s"init of ll list: ${List.init(ll)}")
  println(s"the length of ll: ${List.length(ll)}")

  /*  using fold left
      ex 3.11
   */
  def sum(l: List[Int]): Int = List.foldLeft(l, 0)(_ + _)

  println(s"sum of l with fold left: ${sum(l)}")

  def product(l: List[Int]): Int = List.foldLeft(l, 1)(_ * _)

  println(s"product of ll with fold left: ${product(ll)}")

  def length(l: List[Int]): Int = List.foldLeft(l, 0)((acc, _) => acc + 1)

  println(s"length of l with fold left: ${length(l)}")

  /*
    3.12
   */
  def reverse[A](l: List[A]): List[A] = List.foldLeft(l, List[A]())((rl, e) => Cons(e, rl))

  println(s"reverse of ll : ${reverse(ll)}")

  /*
    3.13
   */
  def foldLeftViaFoldRight_1[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    List.foldRight(l, (b: B) => b) ((a, g) => b => g(f(b, a)))(z)

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    List.foldLeft(reverse(l), z)((b, a) => f(a, b))

  /*
    3.14
   */
  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] =
    List.foldRight(l1, l2)(Cons(_, _))

  /*
    3.15
   */
}