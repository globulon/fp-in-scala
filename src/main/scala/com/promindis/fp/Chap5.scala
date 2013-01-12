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
  }

  val s = Stream(1,2,3)
}
