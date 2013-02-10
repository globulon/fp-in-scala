package com.promindis.fp

import util.matching.Regex

trait Parsers[Parser[+ _]] {
  self =>

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  implicit def string(string: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOp(p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOp[String] = ParserOp[String](f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n == 0) succeed(List.empty[A]) else map2(p, listOfN(n - 1, p)) (_::_)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p) { a => succeed(f(a)) }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p)) (_::_)

  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_::_) or succeed(List.empty[A])

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    flatMap(p) { a =>
      map(p2) { b => f (a, b)}
    }

  def succeed[A](a: A) = string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = map2(p, p2)((_,_))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def  regex[A](r: Regex): Parser[A]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def contextParser = flatMap(regex("""\d""".r)) { n => many1(char('a')) }

  def errorLocation(e: ParseError): Location

  def errorMessage(e: ParseError): String

  def errorStack(m: ParseError): List[(Location, String)]

  def attempt[A](p: Parser[A]): Parser[A]

  case class ParserOp[A](p: Parser[A]) {
    def |[B >: A](pb: Parser[B]): Parser[B] = self.or(p, pb)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]) : Parser[B] = self.flatMap(p)(f)

    def  product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def  **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def or[B >: A](p2: Parser[A]): Parser[B] = self.or(p, p2)
  }
}

case class Location(s: String, offset: Int = 0) {
  def line = s.slice(0, offset + 1).count(_ == '\n') + 1
  def column = s.slice(0, offset + 1).reverse.indexOf('\n')
}

case class ParseError(stack: List[(Location, String)] = List.empty, otherFailures: List[ParseError] = List.empty)