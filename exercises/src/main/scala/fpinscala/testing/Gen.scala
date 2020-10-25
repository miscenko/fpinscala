package fpinscala.testing

import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors, ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  // 8.9
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Passed => p.run(max, n, rng)
        case x => x
      }
  }

  def tag(msg: String): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(errMsg, c) => Falsified(msg + "\n" + errMsg, c)
        case x => x
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
        case x => x
      }
  }
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop(
    (n, rng) => randomStream(as)(rng).zip(LazyList.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  )

  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: %{e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }

}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  // 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  // 8.5
  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  // 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean flatMap ((b: Boolean) => if (b) g1 else g2)

  // 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap(d => if (d < threshold) g1._1.sample else g2._1.sample))
  }

  // 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen { n => g.listOfN(n) }

}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = ???

  // 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap ((n: Int) => this.listOfN(n))

  // 8.10
  def unsized: SGen[A] = SGen(_ => this)
}

case class SGen[+A](g: Int => Gen[A]) {
  // 8.11
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] = SGen {
    g(_) map f
  }

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen {
    n =>
      g(n) flatMap {
        f(_).g(n)
      }
  }
}

