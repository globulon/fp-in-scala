package com.promindis.fp

trait ST[S, A] {
  self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = ST.map(this)(f)

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = ST.flatMap(this)(f)
}

object ST {
  def apply[S, A](a: => A) = new ST[S, A] {
    lazy val cached = a

    def run(s: S) = (a, s)
  }

  def run[A](r: RunnableST[A]) = r[Unit].run(())._1

  def map[S, A, B](sta: ST[S, A])(f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, newS) = sta.run(s)
      (f(a), newS)
    }
  }

  def flatMap[S, A, B](sta: ST[S, A])(f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S) = {
      val (a, newS) = sta.run(s)
      f(a).run(newS)
    }
  }
}

trait STRef[S, A] {
  owner =>
  protected var cell: A

  def write(a: A): ST[S, A] = new ST[S, A] {
    protected def run(s: S): (A, S) = {
      owner.cell = a
      (a, s)
    }
  }

  def read: ST[S, A] = ST[S, A](cell)
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    protected var cell = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

object UseSTRef {
  def main(args: Array[String]) {
    val r: RunnableST[(Int, Int)] = new RunnableST[(Int, Int)] {
      def apply[S] = for {
        r1 <- STRef[S, Int](1)
        r2 <- STRef[S, Int](2)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(x + 1)
        _ <- r2.write(y + 1)
        a <- r1.read
        b <- r2.read
      } yield (a, b)
    }

    println(ST.run(r))
  }
}
