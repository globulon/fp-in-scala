package com.promindis.fp

import Chap6._
import Gen._
import Prop._

trait Status

case object Proven extends Status

case object Unfalsified extends Status

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop.&&(this, p)

  def ||(p: Prop): Prop = Prop.||(this, p)
}

object Prop {
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String
  type Result = Either[FailedCase, (Status, SuccessCount)]

  def &&(p: Prop, q: Prop):  Prop = Prop {
    (n , rng) => p.run(n, rng) match {
      case l @ Left(_) => l
      case Right((_, i)) => q.run(n, rng).right  map {
        case (s, j) => (s, i + j)
      }
    }
  }

  def ||(p: Prop, q: Prop):  Prop = Prop {
    (n , rng) => p.run(n, rng) match {
      case l @ Left(_) => q.run(n, rng)
      case r @ Right(_) => r
    }
  }
}

case class Gen[+A](sample: State[RNG, A], exhaustive: Domain[A]) {
  def map[B](f: A => B) = Gen.map(this)(f)

  def map2[B, C](g: Gen[B])(f: (A, B) => C) = Gen.map2(this, g)(f)

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen.flatMap(this)(f)

}

object Gen {
  type Domain[+ A] = Stream[Option[A]]


  def map[A, B](gen: Gen[A])(f: A => B): Gen[B] =
    Gen(State.map(gen.sample)(f), gen.exhaustive.map(_.map(f)))

  def map2[A, B, C](g: Gen[A], h: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(
      sample = State.map2(g.sample, h.sample)(f),
      exhaustive = Stream.map2(g.exhaustive, h.exhaustive) {
        (o, p) => RichOption.map2(o, p)(f)
      }
    )

  def flatMap[A, B](g: Gen[A])(f: A => Gen[B]): Gen[B] =
    Gen(
      sample = State.flatMap(g.sample) {
        a => f(a).sample
      },
      exhaustive = g.exhaustive.flatMap { a =>
          a.map(f) match {
            case Some(s) => s.exhaustive
            case None => unbounded
          }
      }
    )

  def bounded[A](a: Stream[A]): Domain[A] = a map (Some(_))

  def unbounded: Domain[Nothing] = Stream(None)

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a), Stream.constant(Some(a)))

  val boolean: Gen[Boolean] = Gen(RNG.boolean, Stream(Some(true), Some(false)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(sample = RNG.range(start, stopExclusive),
      exhaustive = bounded(Stream.from(start)).takeWhile(_.get < stopExclusive))

  import scala.math._

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = sameParity(from, to, makeFixParities(from, to))

  private def sameParity(from: Int, to: Int, fixParities: (Int, Int) => (Int, Int)) =
    choose(from, to).flatMap {
      l =>
        choose(from, to) map {
          r => fixParities(l, r)
        }
    }

  private def makeFixParities(from: Int, to: Int): (Int, Int) => (Int, Int) =
    (a, b) => (a, from + ((b + (abs(b - a) % 2)) - from) % (to - from))

  def listOfN[A](g: Gen[A], size: Gen[Int]): Gen[List[A]] = size flatMap {
    size => listOfN(size, g)
  }


  import RichOption._

  /** Generate lists of length n, using the given generator. */
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(
      sample = State.sequence(List.fill(n)(g.sample)),
      exhaustive = Stream.unfold(g.exhaustive) { s =>
          Some((sequence(s.take(n).toList), s.drop(n)))
      }
    )

  /** Between 0 and 1, not including 1. */
  def uniform: Gen[Double] = Gen(sample = RNG.double, exhaustive = unbounded)

  /** Between `i` and `j`, not including `j`. */
  def choose(i: Double, j: Double): Gen[Double] = Gen(
    sample = RNG.range(i, j),
    exhaustive = unbounded
  )

  def union[A](g: Gen[A], h: Gen[A]): Gen[A] =
    Gen(
      sample = State.flatMap(RNG.boolean) {
        if (_) g.sample else h.sample
      },
      exhaustive = Stream.interleave(g.exhaustive, h.exhaustive)
    )

  //    def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] =


  def forAll[A](g: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => {
      def run(i: Int, j: Int, s: Stream[Option[A]], onEnd: Int => Result): Result =
        if (i == j) Right((Unfalsified, i) )
        else s.uncons match {
          case None => onEnd.apply(i)
          case Some((Some(a), rest)) => safely(f(a)) match {
            case Right(b) if b => run(i + 1, j, rest, onEnd)
            case Right(_) => Left("failed test %s" format())
            case Left(m) => Left("exception running test for %s: %s" format(a, m))
          }
        }

      run(0, n / 3, g.exhaustive, (i) => Right((Proven, i))) match {
        case Right((Unfalsified, _)) =>
          run(n / 3, n, bounded(randomStream(g)(rng)), (i) => Right((Unfalsified, i)))
        case r => r
      }
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG) = Stream.unfold(rng) { r => Some(g.sample.apply(r)) }

  def safely[A](a: => A): Either[String, A] =  {
    try {
      Right(a)
    } catch {
      case e: Throwable => Left(e.getMessage)
    }
  }
}

object RichOption {
  def map2[A, B, C](aa: Option[A], bb: Option[B])(f: (A, B) => C): Option[C] =
    aa.flatMap {
      a => bb.map {
        b => f(a, b)
      }
    }

  def traverseL[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.reverse.foldLeft(Some(List.empty[B]): Option[List[B]]) {
      (acc, cur) =>
        map2(f(cur), acc)(_ :: _)
    }

  def sequence[A](as: List[Option[A]]): Option[List[A]] = traverseL(as)(identity)
}

case class SGen[+A](forSize: Int => Gen[A])

object SGen {
  def map[A, B](g: SGen[A])(f: A => B): SGen[B] = SGen[B] { n => g.forSize(n) map(f) }

  //for a size n we execute n tests Gen[A] and then for each we execute again the Gen[B] obtain
  //from  the previous test result  and n
  def flatMap[A, B](g: SGen[A])(f: A => SGen[B]): SGen[B] =
    SGen[B] { n => g.forSize(n).flatMap { a => f(a).forSize(n) } }


  def map2[A, B, C](g1: SGen[A], g2: SGen[B])(f: (A, B) => C): SGen[C] =
    SGen[C] { n => Gen.map2(g1.forSize(n), g2.forSize(n))(f)  }

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen { n => Gen.listOfN(n, g) }
}

