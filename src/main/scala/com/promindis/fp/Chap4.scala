package com.promindis.fp

object Chap4 {
  def failingFn(i: Int): Int = {
    val x: Int = throw new Exception("fail!")
    try {
      val y = 42 + 5
      x + y
    } catch {
      case e: Exception => 43
    }
  }

  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty  else xs.sum / xs.length


  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] =
      this match {
        case Some(value) => Some(f(value))
        case _ => None
      }

    def getOrElse[B >: A](default: => B): B =
      this match {
        case Some(value) => value
        case _ => default
      }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

    def filter(f: A => Boolean): Option[A] =
    flatMap { value =>
      if (f(value)) Some(value)
      else None
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some.apply).getOrElse(ob)

  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]



}
