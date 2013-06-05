package com.promindis.fp

import scala.reflect.ClassTag

trait ST[S, A] {
  self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = ST.map(this)(f)

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = ST.flatMap(this)(f)
}

object ST {
  def apply[S, A](a: => A): ST[S, A] = new ST[S, A] {
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

sealed abstract class STMap[S, K, V] {
  owner =>
  protected def map: scala.collection.mutable.Map[K, V]

  def get(k: K): ST[S, Option[V]] = ST(map.get(k))

  def +=(k: K, v: V): ST[S, Unit] = ST[S, Unit] (owner.map += (k -> v))

  def remove(k: K): ST[S, Option[V]] = ST[S, Option[V]] (owner.map.remove(k))

  def snapshot: ST[S, Map[K, V]] = ST(owner.map.toMap)
}

object STMap {
  def emtpy[S, K, V](): ST[S, STMap[S, K, V]] = ST[S, STMap[S, K, V]] (new STMap[S, K,V] {
    protected val map: collection.mutable.Map[K, V] = collection.mutable.Map.empty[K, V]
  })

  def from[S, K, V](src: Map[K, V]): ST[S, STMap[S, K, V]] = ST[S, STMap[S, K, V]] (new STMap[S, K,V] {
    protected val map: collection.mutable.Map[K, V] = collection.mutable.Map.empty[K, V] ++ src
  })
}

sealed abstract class STArray[S, A](implicit evidence: ClassTag[A]) {
  protected def array: Array[A]

  def size: ST[S, Int] = ST(array.length)

  def write(pos: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    protected def run(s: S) = {
      array(pos) = a
      ((), s)
    }
  }

  def read(pos: Int): ST[S, A] = ST(array(pos))

  def freeze: ST[S, List[A]] = ST(array.toList)

//  def fill(xs: Map[Int,A]): ST[S, Unit] = xs.headOption match {
//    case None => ST(())
//    case Some(e) => for {
//      _ <- write(e._1, e._2)
//      r <- fill(xs - e._1)
//    } yield r
//  }

  def fill(xs: Map[Int,A]): ST[S, Unit] =
    xs.foldRight(ST[S, Unit](())) {
      case ((k, v), st) => st flatMap { _ => write(k, v) }
    }

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()
}

object STArray {
  def apply[S, A : ClassTag](sz: Int, a: A): ST[S, STArray[S, A]] = ST[S, STArray[S, A]] ( new STArray[S, A] {
    protected lazy val array = Array.fill(sz)(a)
  })

  def fromList[S, A : ClassTag](xs: List[A]): ST[S, STArray[S, A]] = ST[S, STArray[S, A]](new STArray[S, A] {
    protected lazy val array = xs.toArray
  })
}

trait STArrays {
  def unit[S]: ST[S, Unit] = ST[S, Unit](())

  def partition[S](arr: STArray[S,Int], l: Int, r: Int, pivot: Int): ST[S,Int] = for {
    p <- arr.read(pivot)
    _ <- arr.swap(pivot, r)
    jr <- STRef[S, Int](l)
    _ <- (l until r).foldLeft(unit[S]) { (st, i) => for {
        _ <- st
        a <- arr.read(i)
        j <- jr.read
        _ <- if (a < p) arr.swap(i, j) flatMap(_ => jr.write(j + 1)) else unit[S]
      } yield ()
    }
    jj <- jr.read
    _ <- arr.swap(jj, r)
  } yield jj

  def qs[S](arr: STArray[S,Int], l: Int, r: Int): ST[S,Unit] =
    if (l < r) {
      for {
        pi <- partition[S](arr, l, r, l + (l - r) / 2)
        _ <- qs[S](arr, l, pi - 1)
        _ <- qs[S](arr, pi + 1, r)
      } yield ()
    } else unit[S]

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.run(new RunnableST[List[Int]] {
      def apply[S] = for {
        arr <- STArray.fromList(xs)
        size <- arr.size
        _ <- qs(arr, 0, size - 1)
        sorted <- arr.freeze
      } yield sorted
    })

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
