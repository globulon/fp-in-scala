package com.promindis.fp

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

object Chap7 {
  type Par[A] = (ExecutorService) => Future[A]

  object Par {
    def unit[A](a: A): Par[A] = (s: ExecutorService) => UnitFuture(a)

    def fork[A](a: => Par[A]): Par[A] = (s: ExecutorService) => {
      s.submit(new Callable[A] {
        def call() = a(s).get
      })
    }

    def async[A](a: => A) = fork(unit(a))

    def run[A](s: ExecutorService)(p: Par[A]): Future[A] = p(s)

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (s: ExecutorService) => {
        val (fa, fb) = (a(s), b(s))
        ComposeFuture(fa, fb)(f)
      }

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit()) {
      (a, _) => f(a)
    }

    def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => fork(unit(f(a)))

    def sortPar(l: Par[List[Int]]): Par[List[Int]] = map2(l, unit(()))((a, _) => a.sorted)

    def product[A, B](pa: Par[A], pb: Par[B]): Par[(A, B)] = (s: ExecutorService) => Pair(pa(s), pb(s))

    def map_[A, B](fa: Par[A])(f: A => B): Par[B] = (s: ExecutorService) => Transform(fa(s))(f)

    def map2_[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      map_(product(a, b)){ pair => f(pair._1, pair._2) }
  }

  case class Transform[A, B](fa: Future[A])(f: A => B) extends Future[B] {
    def cancel(mayInterruptIfRunning: Boolean) = fa.cancel(mayInterruptIfRunning)

    def get() = get(Long.MaxValue, TimeUnit.MILLISECONDS)

    def get(timeout: Long, unit: TimeUnit) =  f(fa.get(timeout, unit))

    def isCancelled = fa.isCancelled

    def isDone = fa.isDone
  }

  case class Pair[A, B](fa: Future[A], fb: Future[B]) extends Future[(A, B)] {
    private var memoize: Option[(A, B)] = None

    def cancel(mayInterruptIfRunning: Boolean) = fa.cancel(mayInterruptIfRunning) || fb.cancel(mayInterruptIfRunning)

    def get() = get(Long.MaxValue, TimeUnit.MILLISECONDS)

    def get(timeout: Long, unit: TimeUnit) = memoize match {
      case Some(pair) => pair
      case None => {
        val start = System.currentTimeMillis
        val a = fa.get(timeout, unit)
        val consumed = unit.convert(System.currentTimeMillis - start, TimeUnit.MILLISECONDS)
        val b = fb.get(timeout - consumed, unit)
        memoize = Some((a, b))
        memoize.get
      }
    }

    def isCancelled() = fa.isCancelled || fb.isCancelled

    def isDone() = fa.isDone && fb.isDone
  }


  case class ComposeFuture[A, B, C](fa: Future[A], fb: Future[B])(f: (A, B) => C) extends Future[C] {
    private var memoize: Option[C] = None

    def cancel(mayInterruptIfRunning: Boolean) = fa.cancel(mayInterruptIfRunning) || fb.cancel(mayInterruptIfRunning)

    def get(): C = get(Long.MaxValue, TimeUnit.SECONDS)

    def get(timeout: Long, unit: TimeUnit): C = memoize match {
      case Some(data) => data
      case _ =>
        val start = now
        val a = fa.get(timeout, unit)
        val elapsed = TimeUnit.MILLISECONDS.convert(now - start, unit)
        val b = fb.get(timeout - elapsed, unit)
        memoize = Some(f(a, b))
        memoize.get
    }

    def now = System.currentTimeMillis

    def isCancelled = fa.isCancelled || fb.isCancelled

    def isDone = fa.isDone && fb.isDone
  }

  case class UnitFuture[A](override val get: A) extends Future[A] {
    def cancel(mayInterruptIfRunning: Boolean) = false

    def get(timeout: Long, unit: TimeUnit) = get

    def isCancelled = false

    def isDone = false
  }


  def sum(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) Par.unit[Int](as.headOption.getOrElse(0))
    else {
      val (l, r) = as.splitAt(as.size / 2)
      Par.map2(sum(l), sum(r))(_ + _)
    }

}
