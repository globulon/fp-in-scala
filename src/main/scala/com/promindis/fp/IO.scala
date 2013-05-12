package com.promindis.fp

import annotation.tailrec
import scala.language.higherKinds

sealed trait IO[F[_], +A] {
  def map[B](f: A => B): IO[F, B] = IO.map(this)(f)

  def flatMap[B](f: A => IO[F, B]): IO[F, B] = IO.flatMap(this)(f)
}

case class Pure[F[_], +A](a: A) extends IO[F, A]

case class Request[F[_], I, +A](req: F[I], receive: I => IO[F, A]) extends IO[F, A]

object IO {
  @tailrec
  def run[F[_], A](r: Run[F])(io: IO[F, A]): A = io match {
    case Pure(a) => a
    case Request(req, rec) => r(req) match {
      case (input, newR) => run(newR)(rec(input))
    }
  }

  def run[F[_], A](mf: Monad[F])(io: IO[F, A]): F[A] = io match {
    case Pure(a) => mf.unit(a)
    case Request(req, rec) => {
      mf.flatMap(req) { x => run(mf)(rec(x)) }
    }
  }

  def map[F[_], A, B](io: IO[F, A])(f: A => B): IO[F, B] = io match {
    case Pure(a) => Pure(f(a))
    case Request(req, rec) => Request(req, rec andThen { x => IO.map(x)(f) } )
  }

  def flatMap[F[_], A, B](io: IO[F, A])(f: A => IO[F, B]): IO[F, B] = io match {
    case Pure(a) => f(a)
    case Request(req, rec) => Request(req, rec andThen { x => IO.flatMap(x)(f) })
  }

  def unit[F[_], A](a: => A): IO[F, A] = Pure(a)

  def monad[F[_]] = new Monad[({type lambda[A] = IO[F, A]})#lambda] {
    override def map[A, B](ma: IO[F, A])(f: (A) => B): IO[F, B] = ma map f

    def flatMap[A, B](ma: IO[F, A])(f: (A) => IO[F, B]): IO[F, B] = ma flatMap f

    def unit[A](a: => A): IO[F, A] = IO.unit(a)

    override def join[A](ma: IO[F, IO[F, A]]): IO[F, A] = ma match {
      case Pure(Pure(a)) => Pure(a)
      case Pure(r @ Request(_, _))  => r
      case Request(req, rec) => Request(req, rec andThen join)
    }
  }

  def apply[A](a: => A): IO[Runnable,A] = Request(Delay(a), Pure.apply(_: A))
}

trait Runnable[A] {
  def run: A
}

object Delay {
  def apply[A](a: => A) = new Runnable[A] {
    def run: A = a

    override def toString = "Delay(%s)" format(a.toString)

  }

  def map[A, B](r: Runnable[A])(f: A => B): Runnable[B]  = new Runnable[B] {
    def run = {
      f(r.run)
    }
  }

  def flatMap[A, B](r: Runnable[A])(f: A => Runnable[B]): Runnable[B]  = new Runnable[B] {
    def run = {
      println("--------")
      f(r.run).run
    }
  }

  val monad: Monad[Runnable] = new Monad[Runnable] {
    def flatMap[A, B](ma: Runnable[A])(f: (A) => Runnable[B]) = Delay.flatMap(ma)(f)

    override def map[A, B](ma: Runnable[A])(f: (A) => B) = Delay.map(ma)(f)

    def unit[A](a: => A) = Delay(a)
  }

}

trait Run[F[_]] {
  def apply[A](fa: F[A]): (A, Run[F])
}

trait CLI {
  sealed trait Console[A]

  case object ReadLine extends Console[Option[String]]

  case class PrintLine(s: String) extends Console[Unit]

  val runConsole: Run[Console] = new Run[Console] {
    def apply[A](fa: Console[A]): (A, Run[Console]) = fa match {
      case ReadLine =>  (Some(readLine()), runConsole)
      case PrintLine(s)=> {
        println(s)
        ((), runConsole)
      }
    }
  }

  //ex 2
  def console(lines: List[String]): Run[Console] = new Run[Console] {
    def apply[A](fa: Console[A]) = fa match {
      case ReadLine =>  lines match {
        case Nil => (None, console(List.empty))
        case h::t => (Some(h), console(t))
      }
      case PrintLine(s) => {
        println(s)
        ((), console(lines))
      }
    }
  }
}

object RunIO {
  def main(args: Array[String]) {
    val F = IO.monad[Runnable]
    import F._
    val s = sequence(List.fill(1)(IO {math.random}).toList)
    println(IO.run(Delay.monad)(s).run)

//    val Request(d, f) = sequence(List(IO {math.random}))
//    println(f(d.run))

  }

}