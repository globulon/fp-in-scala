package com.promindis.fp

import annotation.tailrec


object Chap5 {

  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]

    def isEmpty: Boolean = uncons.isEmpty

    def toList: List[A] = {
      @tailrec
      def iter(acc: List[A], s: Stream[A]): List[A] =
        s.uncons match {
          case Some((h, t)) => iter(h::acc, t)
          case _ => acc
        }

      iter(List.empty, this).reverse
    }

    def take(n: Int): Stream[A] =
      uncons match {
        case Some((h, t)) if ( n > 0) => Stream.cons(h, t.take(n - 1))
        case _ => Stream.empty[A]
      }

//    def takeWhile(p: A  => Boolean): Stream[A] =
//      uncons match {
//        case Some((h, t)) if p(h) => Stream.cons(h, t.takeWhile(p))
//        case _ => Stream.empty
//      }

    def foldRight[B](z: B)(f: (A, => B) => B): B =
      uncons match {
        case Some((h, t)) => f(h, t.foldRight(z)(f))
        case _ => z
      }

    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      foldRight((z, Stream(z))) { (a, p) =>
        val r = f(a, p._1)
        (r, Stream.cons(r, p._2))
      }._2

    def exists(p: A => Boolean): Boolean =
      foldRight(false) { (a, b) => p(a) || b }

    def forAll(p: A => Boolean): Boolean =
      foldRight(true) { (a, b) => p(a) && b }

    def takeWhile(p: A  => Boolean): Stream[A] =
      foldRight(Stream.empty[A]) { (a, s) =>
        if (p(a)) Stream.cons(a, s)
        else s
      }
    //map
    def map[B](f: A => B): Stream[B] =
      foldRight(Stream.empty[B]) { (a, b) =>
        Stream.cons(f(a),b)
      }

    def filter(p: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A]) { (a, s) =>
        if (p(a)) Stream.cons(a, s)
        else s
      }

    def append[B >: A](b: Stream[B]): Stream[B] =
      foldRight(b) { (a, s) => Stream.cons(a, s) }

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty[B]) { (a, s) => f(a).append(s) }

    def map_1[B](f: A => B): Stream[B] =
      Stream.unfold(this) { (s) => s.uncons map(p => (f(p._1), p._2)) }

    def take_1[B](n: Int): Stream[A] =
      Stream.unfold((n, this)) { (s) =>
        s._2.uncons match {
          case Some((h, t)) if (s._1 > 0) => Some((h, (s._1 - 1, t)))
          case _ => None
      }
     }

    def takeWhile_1(p: A => Boolean): Stream[A] =
      Stream.unfold(this) { s =>
        s.uncons match {
          case Some((h,t)) if p(h) => Some((h,t))
          case _ => None
        }
      }

    def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
      Stream.unfold((this, bs)) { s =>
        (s._1.uncons, s._2.uncons) match {
          case (Some((ha, ta)), Some((hb, tb))) => Some(f(ha, hb), (ta, tb))
          case _ => None
        }
      }

    def zip[B](bs: Stream[B]): Stream[(A, B)] = zipWith(bs)((_, _))

    def zipWithAll[B, C](b: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
      Stream.unfold((Some(this): Option[Stream[A]], Some(b): Option[Stream[B]])) { s =>
        s._1 flatMap { as =>
          s._2.map { bs =>
            (f(as.uncons.map(_._1), bs.uncons.map(_._1)), (as.uncons.map(_._2), bs.uncons.map(_._2)))
          }
        }
      }

    def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = zipWithAll(bs) ((_, _))

    def tails: Stream[Stream[A]] = Stream.tails(this)
  }

  object Stream {
    private object e extends Stream[Nothing] {
      def uncons = None
    }

    def empty[A]: Stream[A] = e

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case Some((a, zz)) => Stream.cons(a, unfold(zz)(f))
        case _ => Stream.empty
      }

  def startsWith[A](s: Stream[A], s1: Stream[A]): Boolean =
     !s.zipAll(s1).takeWhile(_._2.isDefined).exists { p => p._1 != p._2 }

  def tails[A](stream: Stream[A]): Stream[Stream[A]] =
    unfold(stream) { s =>
      s.uncons.map (p => (s, p._2))
    }

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean =
    s1.tails exists (startsWith(_,s2))

  }

  val s = Stream(1,2,3)


//  val ones: Stream[Int] = Stream.cons(1, ones)
//
//  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
//
//  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
//
//  def makeFib(a: Int, b: Int): Stream[Int] = Stream.cons(a, makeFib(b, a + b))
//
//  val fibs = makeFib(0, 1)

  def constant[A](a: A) : Stream[A] = Stream.unfold(a) { (s) => Some((s, s))}

  val ones: Stream[Int] = constant(1)

  def from(n: Int): Stream[Int] = Stream.unfold(n) { (s) => Some((s, s + 1)) }

  val fibs: Stream[Int] = Stream.unfold((0, 1)) { (s) => Some((s._1, (s._2, s._1 + s._2))) }


}