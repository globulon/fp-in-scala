package com.promindis.fp

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
