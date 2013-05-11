package com.promindis.fp

sealed trait Trampoline[+A]

case class Done[+A](get: A) extends Trampoline[A]

case class More[+A](force: () => Trampoline[A]) extends Trampoline[A]

case class Bind[A, +B](force: () => Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]

object Trampoline {
  @annotation.tailrec
  def run[A](t: Trampoline[A]): A = t match {
    case Done(a) => a
    case More(force) => run(force())
    case Bind(force, f) => run(flatMap(force())(f))
  }

  def unit[A](a: => A): Trampoline[A] = More { () => Done(a) }


  def flatMap[A, B](ta: Trampoline[A])(f: A => Trampoline[B]): Trampoline[B] = ta match {
    case Done(a) => f(a)
    case More(force) => Bind(force, f)
    case Bind(force, cont) => More { () => Bind(force, cont andThen (flatMap(_)(f))) }
  }
}