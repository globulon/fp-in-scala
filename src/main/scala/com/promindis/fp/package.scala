package com.promindis

package object fp {
  type Pred[A] = A => Boolean

  private def abs(n: Int) = if (n >= 0) n else -n

  def not[A](f: A => Boolean): A => Boolean = (!f(_))

  def absolute[A](f: A => Int) =  (n: A) => abs(f(n))

  def lift [A, B, C, D](f: (B ,C) => D)(
                        g: A => B,
                        h: A => C): A => D = (a: A) => f(g(a), h(a))

  def curry[A, B, C](f: (A, B) => C): A => B => C = (a: A) => f(a, _)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C  = (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g : A => B): A => C = (a: A) => f(g(a))

//  def lift3[A,B,C,D,E](f: (B, C, D) => E)(g: A => B, h: A => C,  i: A => D): A => E = (a: A) => f(g(a), h(a), i(a))
  def lift3[A,B,C,D,E](f: (B, C, D) => E)(g: A => B, h: A => C,  i: A => D): A => E =
    (a: A) => (lift((c: C, d: D) => f(g(a), c, d))(h, i))(a)
}
