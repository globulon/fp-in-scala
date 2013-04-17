package com.promindis.fp

sealed trait Validation[+E, +A]

case class Failure[E](tail: Seq[E]) extends Validation[E, Nothing]

object Failure {
  def apply[E, A](e: E): Validation[E, A] = Failure(Seq(e))
}

case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {
  def map[E, A, B](va: Validation[E, A])(f: A => B): Validation[E, B] = va match {
    case Success(v) => Success(f(v))
    case Failure(t) => Failure(t)
  }

  def flatMap[E, EE >: E, A, B](va: Validation[E, A])(f: A => Validation[EE, B]): Validation[EE, B] = va match {
    case Success(v) => f(v)
    case Failure(t) => Failure(t)
  }

  def apply[E, A, B](vf: Validation[E, A => B])(va: Validation[E, A]): Validation[E, B] = (va, vf) match {
    case (Success(a), Success(f)) => Success(f(a))
    case (Success(_), Failure(h)) => Failure(h)
    case (Failure(s), Success(_)) => Failure(s)
    case (Failure(sa), Failure(sb)) => Failure(sa ++ sb)
  }

  def applicative[E] = new Applicative[({type lambda[A] = Validation[E, A]})#lambda] {
    override def apply[A, B](fab: Validation[E, (A) => B])(fa: Validation[E, A]) = Validation.apply(fab)(fa)

    def unit[A](a: => A) = Success(a)

    override def map[A, B](va: Validation[E, A])(f: (A) => B) = Validation.map(va)(f)
  }
}