package com.promindis.fp

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id.map(this)(f)

  def flatMap[B](f: A => Id[B]): Id[B] = Id.flatMap(this)(f)
}

object Id {
  def map[A, B](ia: Id[A])(f: A => B): Id[B] = Id(f(ia.value))

  def flatMap[A, B](ia: Id[A])(f: A => Id[B]): Id[B] = f(ia.value)
}

object MonadId extends Monad[Id] {
  def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]) = ma flatMap(f)

  override def map[A, B](ma: Id[A])(f: (A) => B) = ma map (f)

  def unit[A](a: => A) = Id(a)
}