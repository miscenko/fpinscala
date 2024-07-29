package fpinscala.errorhandling

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object OptionNew extends App {
  //val opt = Some("kuku")
  //  println(opt.map(_.length))
  //  println(None.getOrElse(42))
  //  println(None.flatMap(a => Some(a)))
  //  println(Some(32).flatMap(a => Some(a + 10)))
  //  println(None.orElse(Some("lala")))
  //  println(Some(23).orElse(Some("lala")))
  //  println(None.filter(a => true))
  //  println(Some(23).filter(a => a < 30))

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.size)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a => b.map(b => f(a, b)))

  // 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t).map(hh :: _))
  }

  val lOpts = List(Some(1), Some(2), Some(3))
  val lOpts2 = List(Some(1), None, Some(3))

  //  println(sequence(lOpts))
  //  println(sequence(lOpts2))

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h) flatMap (hh => traverse(t)(f).map(hh :: _))
  }

  val ll = List(1, 2, 3)
  println(traverse(ll)(n => Some(n + 1)))
  println(traverse(ll)(n => if (n == 1) Some(n + 1) else None))
}
