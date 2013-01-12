package com.promindis.fp

import java.util.regex.{PatternSyntaxException, Pattern}

object Chap4 {
  def failingFn(i: Int): Int = {
    val x: Int = throw new Exception("fail!")
    try {
      val y = 42 + 5
      x + y
    } catch {
      case e:  Exception => 43
    }
  }
  //////Option definition///////////
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

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap { x =>  b map { y => f(x, y) } }


  /////\\\\\\\Option definition///////////

  def mean(xs: Seq[Double]): Option[Double] =  if (xs.isEmpty) None  else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m =>
      mean(xs.map(v => math.pow(m - v, 2)))
    }

  def pattern(r: String): Option[Pattern] =
    try { Some(Pattern.compile(r)) }
    catch {
      case e: PatternSyntaxException => None
    }

  def makeMatcher(r: String): Option[String => Boolean] =
    pattern(r) map { p => (s: String) => p.matcher(s).matches() }

  def doesMatch(p: String, s: String): Option[Boolean] = makeMatcher(p) map { m => m(s) }

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    map2(makeMatcher(pat), makeMatcher(pat2))  { (f, g) =>
      f(s) && g(s)
    }

//  def sequence[A](a: List[Option[A]]): Option[List[A]] =
//    a.reverse.foldLeft(Some(List.empty[A]): Option[List[A]]) { (acc, cur) =>
//      map2(acc, cur) { (b, c) => c::b }
//    }

  def traverseR[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(List.empty[B]): Option[List[B]]) { (cur, acc) =>
      map2(f(cur), acc) { (x, xs) => x::xs }
    }

  def traverseL[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.reverse.foldLeft(Some(List.empty[B]): Option[List[B]]) { (acc, cur) =>
      map2(f(cur), acc) { (x, xs) => x::xs }
    }


  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverseL(a)(identity)

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

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      this flatMap { x =>
        b map (f(x, _))
      }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def traverse[A, E, B](xs: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    xs.foldRight(Right(List.empty): Either[E, List[B]]) { (cur, acc) =>
      f(cur).map2(acc)(_::_)
    }

  def sequence[E, A](xs: List[Either[E, A]]): Either[E, List[A]] = traverse(xs)(identity)

//  println(sequence(List(Right(1), Right(2), Right(3))))
//  println(sequence(List(Right(1), Left("Unexpected"), Right(3))))
}
