package com.promindis.fp

case class Mutate[A, B](set: (A, B) => A) {
  def apply(a: A, b: B) = set(a, b)
}

object Mutate {

  def map[A, B, C](m: Mutate[A, B])(f: A => A): Mutate[A, B] = Mutate {
    (a, b) => f(m(a, b))
  }

  def flatMap[A, B, C](m: Mutate[A, B])(f: A => Mutate[A, B]): Mutate[A, B] = Mutate {
    (a, b) => f(m(a, b))(a,b)
  }


}
