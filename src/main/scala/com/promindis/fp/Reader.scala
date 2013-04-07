package com.promindis.fp

case class Reader[R, A](run: R => A) {
  def map[B](f: A => B): Reader[R, B] = Reader.map(this)(f)

  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = Reader.flatMap(this)(f)
}

object Reader {
  def map[R, A, B](ra: Reader[R, A])(f: A => B): Reader[R, B] = Reader {
    r => f(ra.run(r))
  }

  def flatMap[R, A, B](ra: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader {
    r => f(ra.run(r)).run(r)
  }

  def monad[R] = new Monad[({type lambda[A] = Reader[R, A]})#lambda] {
    def unit[A](a: => A): Reader[R, A] = Reader { _ => a }

    def flatMap[A, B](ma: Reader[R, A])(f: (A) => Reader[R, B]) = Reader.flatMap(ma)(f)

    override def map[A, B](ma: Reader[R, A])(f: A => B) = Reader.map(ma)(f)
  }

  def ask[R] = Reader[R, R] (identity)
}