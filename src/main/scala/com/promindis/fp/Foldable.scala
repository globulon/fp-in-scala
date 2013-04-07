package com.promindis.fp

import annotation.tailrec
import scala.language.higherKinds

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] = foldLeft(fa)(List.empty[A]){ (acc, item) => (item::acc) }
}

object Foldable {

  val list = new Foldable[List] {
    def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B = {
      @tailrec
      def iter(acc: B, rem: List[A]): B  =
        rem match {
          case Nil => acc
          case head::tail => iter(mb.op(acc, f(head)), tail)
        }

      iter(mb.zero, as)
    }

    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
      foldMap(as)((a: A) => (b:  B) => f(b, a))(Monoid.endoMonoid[B])(z)

    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
      foldMap(as)(f.curried(_))(Monoid.dual(Monoid.endoMonoid[B]))(z)
  }

  val indexedSeq = new Foldable[IndexedSeq] {
    def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B =
      as.size match {
        case s if s == 0 => mb.zero
        case s if s == 1 => f(as.head)
        case s => {
          val (l, r) = as splitAt (s / 2)
          mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
        }
      }

    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
      foldMap(as)((a: A) => (b:  B) => f(b, a))(Monoid.endoMonoid[B])(z)

    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
      foldMap(as)(f.curried(_))(Monoid.dual(Monoid.endoMonoid[B]))(z)
  }

  val stream = new Foldable[Stream] {
    def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]) = {
      @tailrec
      def iter(acc: B, rem: Stream[A]): B  =
        rem.uncons match {
          case None => acc
          case Some((head, tail)) => iter(mb.op(acc, f(head)), tail)
        }

      iter(mb.zero, as)
    }

    def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
      foldMap(as)((a: A) => (b:  B) => f(b, a))(Monoid.endoMonoid[B])(z)

    def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
      foldMap(as)(f.curried(_))(Monoid.dual(Monoid.endoMonoid[B]))(z)
  }

  val tree = new Foldable[Tree] {
    def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B =
      as match {
        case Leaf(v) => f(v)
        case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
      }

    def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(v) => f(z, v)
        case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
      }

    def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Leaf(v) => f(v, z)
        case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
      }
  }

  val option = new Foldable[Option] {
    def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
        case None => mb.zero
        case Some(v) => f(v)
      }

    def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = as match {
      case None => z
      case Some(v) => f(z, v)
    }

    def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) = as match {
        case None => z
        case Some(v) => f(v, z)
      }

  }
}

