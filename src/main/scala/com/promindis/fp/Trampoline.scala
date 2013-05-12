package com.promindis.fp

import scala.annotation.tailrec

sealed trait Trampoline[+A]

case class Done[+A](get: A) extends Trampoline[A]

case class More[+A](force: () => Trampoline[A]) extends Trampoline[A]

case class Bind[A, +B](force: () => Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]

object Trampoline {

  @tailrec
  final def run[A](ta: Trampoline[A]): A = ta match {
    case Done(get) => get
    case More(force) => run(force())
    case Bind(force, f) => run(flatMap(force())(f) )
  }

  def map[A, B](ta: Trampoline[A])(f: A => B): Trampoline[B] = ta match {
    case Done(a) => Done(f(a))
    case More(force) => Bind(force, f andThen unit[B])
    case Bind(force, g) => More { () => Bind(force, g andThen {tt => map(tt)(f) } ) }
  }

  def flatMap[A, B](ta: Trampoline[A])(f: A => Trampoline[B]): Trampoline[B] = ta match {
    case Done(a) => f(a)
    case More(force) => Bind(force, f)
    case Bind(force, g) => More { () => Bind(force, g andThen { tt => flatMap(tt)(f) } ) }
  }

  def unit[A](a: A): Trampoline[A] = Done(a)


  val monad = new Monad[Trampoline] {
    def flatMap[A, B](ma: Trampoline[A])(f: (A) => Trampoline[B]) = Trampoline.flatMap(ma)(f)


    override def map[A, B](fa: Trampoline[A])(f: (A) => B) = Trampoline.map(fa)(f)

    def unit[A](a: => A) = Trampoline.unit(a)
  }
}