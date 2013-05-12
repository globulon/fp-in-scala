package com.promindis.fp

import scala.annotation.tailrec

trait TailRec {
  sealed trait Bounce[+A]

  case class Done[+A](get: A) extends Bounce[A]

  case class Call[A](f: () => Bounce[A])extends Bounce[A]

  @tailrec
  final protected def trampoline[A](ba: Bounce[A]): A = ba match {
    case Done(get) => get
    case Call(f) => trampoline(f())
  }
}

object RunTailRec extends TailRec {
  private def isEven(n: Int) : Bounce[Boolean] = {
    if (n == 0)  Done(true)
    else Call(() => isEven2(n - 1))
  }

  private def isEven2(n: Int): Bounce[Boolean] = {
    if (n == 0) Done(false)
    else Call(() => isEven(n - 1))
  }

  def main(args: Array[String]) {
    println(trampoline(isEven(99998)))
  }
}
