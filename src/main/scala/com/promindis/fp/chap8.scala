package com.promindis.fp

object Chap8 {
  import Chap6._

  type Domain[+A] = Stream[Option[A]]

  case class Gen[+A](sample: State[RNG, A], exhaustive: Domain[A]) {
    def map[B](f : A => B) = Gen.map(this)(f)

    def map2[B, C] (g: Gen[B])(f: (A, B) => C) = Gen.map2(this, g)(f)

    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen.flatMap(this)(f)

  }

  object Gen {

    def map[A, B](gen: Gen[A])(f: A => B): Gen[B] =
      Gen(State.map(gen.sample)(f), gen.exhaustive.map(_.map(f)))

    def map2[A, B, C](g: Gen[A], h: Gen[B])(f: (A, B) => C): Gen[C] =
      Gen(
        sample = State.map2(g.sample, h.sample)(f),
        exhaustive = Stream.map2(g.exhaustive, h.exhaustive) { (o, p) => RichOption.map2(o, p)(f)}
      )

    def flatMap[A, B](g: Gen[A])(f: A => Gen[B]): Gen[B] =
      Gen(
        sample = State.flatMap(g.sample) { a => f(a).sample },
        exhaustive =   g.exhaustive.flatMap { a =>
          a.map(f) match  {
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

    def sameParity(from: Int, to: Int): Gen[(Int,Int)] = sameParity(from, to, makeFixParities(from, to))

    private def sameParity(from: Int, to: Int, fixParities: (Int, Int) => (Int, Int)) =
      choose(from, to).flatMap { l =>
        choose(from, to) map  { r => fixParities(l, r) }
      }

    private def makeFixParities(from: Int, to: Int): (Int, Int) => (Int, Int) =
      (a, b) => (a, from + ((b + (abs(b - a) % 2)) - from) % (to - from))

    def listOfN[A](g: Gen[A], size: Gen[Int]): Gen[List[A]] = size flatMap { size => listOfN(size, g) }


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
    def uniform: Gen[Double] = Gen( sample = RNG.double, exhaustive = unbounded )

    /** Between `i` and `j`, not including `j`. */
    def choose(i: Double, j: Double): Gen[Double] = Gen(
      sample = RNG.range(i, j),
      exhaustive = unbounded
    )

    def union[A](g: Gen[A], h: Gen[A]): Gen[A] =
      Gen(
        sample = State.flatMap(RNG.boolean){ if(_) g.sample else h.sample },
        exhaustive = Stream.interleave(g.exhaustive, h.exhaustive)
      )
  }

  object RichOption {
    def map2[A, B, C](aa: Option[A], bb: Option[B])(f: (A, B) => C): Option[C] =
      aa.flatMap { a => bb.map{ b => f(a, b) } }

    def traverseL[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
      as.reverse.foldLeft(Some(List.empty[B]): Option[List[B]]) { (acc, cur) =>
        map2(f(cur), acc) (_::_)
      }

    def sequence[A](as: List[Option[A]]): Option[List[A]] = traverseL(as)(identity)

  }
}
