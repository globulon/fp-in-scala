package com.promindis.fp

sealed trait Either[+E, +A]  {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case l @ Left(_) => l
      case Right(v) => Right(f(v))
    }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case l @ Left(_) => b
      case r @ Right(_) => r
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    map(f) match {
      case Right(l @ Left(_)) => l
      case Right(r) => r
      case l @ Left(_) => l
    }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def monad[E] = new Monad[({type lambda[A] = Either[E, A]})#lambda] {
    def flatMap[A, B](ma: Either[E, A])(f: (A) => Either[E, B]) = ma flatMap(f)

    override def map[A, B](ma: Either[E, A])(f: (A) => B): Either[E, B] = ma map f

    override def join[A](ma: Either[E, Either[E, A]]): Either[E, A] = ma match {
      case Right( r @ Right(_)) => r
      case Right(l @ Left(_)) => l
      case l @ Left(_) => l
    }

    def unit[A](a: => A) = Right(a)
  }
}