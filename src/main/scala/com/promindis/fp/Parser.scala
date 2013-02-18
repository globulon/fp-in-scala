package com.promindis.fp

import util.matching.Regex
import com.promindis.fp.JSON._
import com.promindis.fp.JSON.JBool
import com.promindis.fp.JSON.JNumber

trait Parsers[Parser[+ _]] {
  self =>

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  implicit def string(string: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOp(p)

  def any: Parser[Char] = regex(".".r).slice.map(_.charAt(0))

  def number: Parser[Double]

  def boolean: Parser[Boolean]

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

  def within[A, B](open: Parser[A], close: Parser[A])(p: => Parser[B]): Parser[B]

  def between[A, B](exp: Parser[A])(p: => Parser[B]): Parser[B] = within(exp, exp)(p)

  case class ParserOp[A](p: Parser[A]) {
    def |[B >: A](pb: Parser[B]): Parser[B] = self.or(p, pb)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]) : Parser[B] = self.flatMap(p)(f)

    def  product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def  **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def or[B >: A](p2: Parser[A]): Parser[B] = self.or(p, p2)

    def slice: Parser[String] = self.slice(p)
  }
}

case class Location(s: String, offset: Int = 0) {
  def line = s.slice(0, offset + 1).count(_ == '\n') + 1
  def column = s.slice(0, offset + 1).reverse.indexOf('\n')
}

case class ParseError(stack: List[(Location, String)] = List.empty, otherFailures: List[ParseError] = List.empty)

object Parsers {
  def jsonParser[Parser[+ _]](parsers: Parsers[Parser]): Parser[JSON] = {
    import parsers._

    val spaces = char(' ').many.slice

    val openPar = char('(')

    var closePar = char(')')

    val openCurly = char('{')

    val closeCurly = char('}')

    val openSquare = char('[')

    val closeSquare = char(']')

    val quote = char('"')

    val jstring: Parser[JSON] = between(quote)(any.many.slice) map JString

    val jnumber: Parser[JSON] = number map JNumber

    val jbool: Parser[JSON] = boolean map JBool

    val jnull: Parser[JSON] = string("null") map (_ => JNull)

    val jarray: Parser[JSON] = null

    val jobject: Parser[JSON]  = null



    jarray or jobject
  }
}