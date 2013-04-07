package com.promindis.fp

trait IO[+A] {
  self =>
  def run: A

  def ++[B >: A](io: IO[B]): IO[B] = new IO[B] {
    def run = {
      self.run
      io.run
    }
  }

  def map[B](f: A => B): IO[B]  = IO.map(self)(f)

  def flatMap[B](f: A => IO[B]): IO[B] = IO.flatMap(self)(f)
}

object IO {
  val empty: IO[Unit] = unit(())

  def map[A, B](io: IO[A])(f: A => B): IO[B] = new IO[B] {
    def run = f(io.run)
  }

  def flatMap[A, B](io: IO[A])(f: A => IO[B]): IO[B] = new IO[B] {
    def run = f(io.run).run
  }


  def unit[A](a: => A) = new IO[A] {
    def run = a
  }

  def monad[A]: Monad[IO] = new Monad[IO] {
    override def map[A, B](ma: IO[A])(f: (A) => B) = IO.map(ma)(f)

    def flatMap[A, B](ma: IO[A])(f: (A) => IO[B]) = IO.flatMap(ma)(f)

    def unit[A](a: => A) = IO.unit(a)
  }
}


trait Converter {
  protected def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0/9.0

  def ReadLine: IO[String] = new IO[String] {
    def run = readLine()
  }

  def PrintLine(message: String): IO[Unit] = new IO[Unit] {
    def run { println(message) }
  }

  val converter: IO[Double] = for {
    _ <- PrintLine("Please provides a Temp in Farenheit")
    t <- ReadLine.map(_.toDouble)
    _ <- PrintLine("Converted %s F in Celsius: %s" format (t, fahrenheitToCelsius(t)))
  } yield t
}


object RunConverter extends Converter {
  def main(args: Array[String]) {
    converter.run
  }
}
