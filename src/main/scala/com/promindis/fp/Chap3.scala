package com.promindis.fp

import annotation.tailrec

object Chap3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A](list: List[A]) = list match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

    @tailrec
    def drop[A](list: List[A], n: Int): List[A] = list match {
      case Nil => Nil
      case _ if n == 0 => list
      case Cons(_, xs) => drop(xs, n - 1)
    }

    @tailrec
    def dropWhile[A](list: List[A])(p: A => Boolean): List[A] = list match {
      case Nil => Nil
      case Cons(x, xs) if p(x) => dropWhile(xs)(p)
      case _ => list
    }

    def setHead[A](value: A, list: List[A]): List[A] = list match {
      case Nil => Nil
      case Cons(_, xs) => Cons(value, xs)
    }

    def empty[A]: List[A] = Nil

    def reverse[A](l: List[A]): List[A] = foldLeft(l, empty[A])((xs, x) => Cons(x, xs))

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B =
      list match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    @tailrec
    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
      l match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }

    def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

    def prod(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

    def foldRight2[A, B](list: List[A], z: B, p: A => Boolean)(f: (A, B) => B): B =
      list match {
        case Nil => z
        case Cons(x, xs) if p(x) => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    //Note EXO 9 builds a list from a List

    def sumRight(list: List[Int]): Int = foldRight(list, 0)(_ + _)

    def productRight(list: List[Int]): Int = foldRight(list, 1)(_ * _)

    def length[A](l: List[A]) = foldRight(l, 0) {
      (_, b) => b + 1
    }

    //linear but not tailrec
    def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

    def concatenate[A](ll: List[List[A]]): List[A] = foldRight(ll, empty[A])(append(_, _))

    def foldRightAsLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(l), z) { (acc , item) => f(item, acc) }

    def foldLeftViaRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
      foldRight(l, (b: B) => b) { (item, acc) =>
        (b: B) => acc(f(b, item))
      }.apply(z)

    def addOneToList(l: List[Int]): List[Int] =
      foldRight(l, empty[Int])((item, acc) => Cons(item + 1, acc))

    def listToString[A](l: List[A]): List[String] =
      foldRight(l, empty[String])((item, acc) => Cons(item.toString, acc))

    def map[A,B](l: List[A])(f: A => B): List[B] =
      foldLeft(reverse(l), empty[B])((acc, item) => Cons(f(item), acc))

    def filter[A](l: List[A])(p: A => Boolean): List[A] =
      foldLeft(reverse(l), empty[A]){(acc, item) =>
        if (p(item)) acc
        else Cons(item, acc)
     }

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = concatenate(map(l)(f))

    private def single[A](a: A) = Cons(a, Nil)

    def filterFlatMap[A](l: List[A])(p: A => Boolean): List[A] =
      flatMap(l) { item => if (p(item)) empty[A] else single(item) }

    def merge[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
      @tailrec
      def iter(rem1: List[A], rem2: List[B], acc: List[C]): List[C] =
        rem1 match {
          case Cons(x, xs) =>
              rem2 match {
                case Cons(y, ys) => iter(xs, ys, Cons(f(x, y), acc))
                case _ => acc
              }
          case _ => acc
        }

      reverse(iter(l1, l2, empty[C]))
    }

    def addLists(l1: List[Int], l2: List[Int]) = merge(l1,l2)(_ + _)

  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
//    def size[A](t: Tree[A]): Int =
//      t match {
//        case Leaf(_) => 1
//        case Branch(left, right) => size(left) + size(right)
//      }
//
//    def maximum(t: Tree[Int]): Int =
//        t match {
//          case Leaf(data: Int) => data
//          case Branch(left, right) => scala.math.max(maximum(left), maximum(right))
//        }
//
//    def depth[A](t: Tree[A]): Int =
//      t match {
//        case Leaf(_) => 1
//        case Branch(left, right) => 1 + depth(left).max(depth(right))
//      }
//
//    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
//      t match {
//        case Leaf(v) => Leaf(f(v))
//        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
//      }

    def fold[A, B](t: Tree[A])(f: (A) => B)(g: (B, B) => B): B =
      t match {
        case Leaf(v) => f(v)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
      }

    def size[A](t: Tree[A]): Int  = fold(t)(v => 1)(_ + _)

    def maximum(t: Tree[Int]): Int = fold(t)(identity)(scala.math.max(_, _))

    def depth[A](t: Tree[A]): Int = fold(t)(v => 1) ( scala.math.max(_, _) + 1 )

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)( v => Leaf(f(v)))(Branch(_, _))
  }

  val aTree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(5)), Leaf(7)))
}
