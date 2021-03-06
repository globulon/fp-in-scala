package com.promindis.fp

import annotation.tailrec
import scala.language.implicitConversions

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    override val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]]  = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    def zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2

    def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2

    def zero = 1
  }

  val booleanOr: Monoid[Boolean]  = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2

    def zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2

    def zero = true
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    def op(a1: (A) => A, a2: (A) => A) = a1 andThen a2

    def zero = identity[A]
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y
    val zero = None
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A): A = m.op(y, x)
    val zero = m.zero
  }

  val wordsMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String) = "%s %s" format(a1.trim, a2.trim)

    override val zero = ""
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  //2 pass
  def foldMap1[A,B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as map f, m)

  //1 pass
  def foldMap2[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero) { (acc, item) => m.op(acc, f(item)) }

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B =  {
    @tailrec
    def iter(acc: B, remaining: List[A]): B = remaining match {
      case head::tail => iter(m.op(acc, f(head)), tail)
      case Nil => acc
    }

    iter(m.zero, as)
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A,B) => B): B = foldMap(as, dual(endoMonoid[B]))(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B,A) => B): B = foldMap(as, endoMonoid[B])(a => b => f(b, a))(z)

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
      case (Stub(s), Part(l, count, r)) => Part(s + l, count, r)
      case (Part(l, count, r), Stub(s)) => Part(s, count, r + s)
      case (Part(l1, count1, r1), Part(l2, count2, r2)) =>
        Part(l1 , count1 + count2 + (if ((r1 + l2).isEmpty) 0 else 1),  r2)
    }

    val zero: WC = Stub("")
  }

  def count(s: String): Int = {

    val toWC: (Char) => WC = {
      case c if c.isWhitespace => Part("", 0, "")
      case c => Stub(c.toString)
    }

    def divideAndConquer(str: String): WC = str match {
      case input if input.isEmpty => wcMonoid.zero
      case input if input.length == 1 => toWC(input.head)
      case input => {
        val (l, r) = input.splitAt(input.length / 2)
        wcMonoid.op(divideAndConquer(l), divideAndConquer(r))
      }
    }

    def stubCount: (Stub) => Int = {
      case Stub(r) if r.isEmpty => 0
      case Stub(r)  => 1
    }

    implicit def toStub(s: String): Stub = Stub(s)

    divideAndConquer(s) match {
      case r @ Stub(_)  => stubCount(r)
      case Part(l, wc, r) => stubCount(l) + wc + stubCount(r)
    }
  }

  type OrderingSlice[A] = (A, A, Boolean)
  def min[A](slice: OrderingSlice[A]) = slice._1
  def max[A](slice: OrderingSlice[A]) = slice._2
  def isOrdered[_](slice: OrderingSlice[_]): Boolean = slice._3

  object OrderingSlice {
    def apply[A](min: A, max: A, ordered: Boolean) = (min, max, ordered)
  }


  def orderMonoid[A : Ordering] = new Monoid[Option[OrderingSlice[A]]] {
    def op(a1: Option[OrderingSlice[A]], a2: Option[OrderingSlice[A]]) = (a1, a2) match {
      case (None, r) => r
      case (l, None) => l
      case (Some(slice1), Some(slice2)) =>
        val o = implicitly[Ordering[A]]
        Some((
          o min(min(slice1), min(slice2)),
          o max(max(slice1), max(slice2)),
          isOrdered(slice1) && isOrdered(slice2) && (o lteq(max(slice1), min(slice2)))
        ))
    }

    def zero = None
  }


  def foldMapV[A,B](v: Traversable[A], m: Monoid[B])(f: A => B): B = v.size match {
    case 0 => m.zero
    case 1 => f(v.head)
    case length => {
      val (l, r) = v splitAt (length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  def ordered[A : Ordering](seq: Traversable[A]): Boolean =
    foldMapV(seq, orderMonoid[A]) {
      case item => Some(OrderingSlice(item, item, ordered = true))
    } match {
      case None => true
      case Some(slice) => isOrdered(slice)
    }


  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A,B)] = new Monoid[(A,B)] {
    def op(a1: (A, B), a2: (A, B)) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

    def zero = (A.zero, B.zero)
  }

//  def coproductMonoid[A,B](A: Monoid[A],
//                           B: Monoid[B]): Monoid[Either[A,B]] = new Monoid[Either[A,B]] {
//    def op(a1: Either[A, B], a2: Either[A, B]) = null
//
//    def zero = Right(B.zero)
//  }

  def mergeMapMonoid[K,V](m: Monoid[V]): Monoid[Map[K,V]] = new Monoid[Map[K,V]] {
    def op(a1: Map[K, V], a2: Map[K, V]) = a2 ++ (a1 map {
        case (k,v) => (k, m.op(v, a2 get(k) getOrElse (m.zero)))
      })

    def zero = Map.empty[K,V]
  }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(a1: (A) => B, a2: (A) => B): (A) => B =  a => B.op(a1(a), a2(a))

    def zero: (A) => B = a => B.zero
  }

//  def frequencyMap(strings: IndexedSeq[String]): Map[String, Int]
  val wordFrequencyMonoid: Monoid[Map[String, Int]] = mergeMapMonoid(intAddition)

  def countFrequencies(str: String): Map[String, Int] =
  Foldable.indexedSeq.foldMap(str.split(" "))(s => Map(s -> 1))(wordFrequencyMonoid)

  def frequencyMap(strings: IndexedSeq[String]): Map[String, Int] =
    Foldable.indexedSeq.foldMap(strings)(countFrequencies(_))(wordFrequencyMonoid)
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC
