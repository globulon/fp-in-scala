package com.promindis.fp

import annotation.tailrec

object Math {
  val positive = (n: Int) => n > 0

  val negative = not(positive)

  def divisibleBy(k: Int): Pred[Int] = (n: Int) => (k % k) == 0

  val even = divisibleBy(2)

  val odd = not(even)

  def divBy3And5(n: Int) = lift((a: Boolean, b: Boolean) => a && b)(divisibleBy(3), divisibleBy(5))

  def divBy3Or5(n: Int) = lift((a: Boolean, b: Boolean) => a || b)( divisibleBy(3), divisibleBy(5))

  def fib(n: Int): Int = {

    @tailrec
    def loop(a: Int, b: Int, rem: Int): Int =
      if  (rem == 0) b
      else loop(b, a + b, rem - 1)

    if (n < 3) (n - 1)
    else loop(0, 1, n - 2)
  }

  def sqrt(n: Double): Double = {
    def f(x: Double) = (x * x) - n
    iterateWhile(2.0)(x => x - f(x) / (2 * x), x => f(x).abs > 1e-14)
  }

  @tailrec
  def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A =
    if (!p(a)) a
    else iterateWhile(f(a))(f, p)

}
