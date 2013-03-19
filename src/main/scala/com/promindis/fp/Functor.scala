package com.promindis.fp

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](f: F[(A, B)]): (F[A], F[B]) = (map(f)(_._1), map(f)(_._2))

}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa.map(f)
  }

}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]

  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))

  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldLeft(unit(List.empty[B])) { (acc, item) => map2(f(item), acc) (_ :: _) }

  def sequence[A](lma: List[M[A]]): M[List[A]] = traverse(lma)(identity[M[A]])

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))

  def factor[A,B](ma: M[A], mb: M[B]): M[(A, B)] = map2(ma, mb)((_, _))

  def cofactor[A,B](e: Either[M[A], M[B]]): M[Either[A, B]] =
    e match {
      case Left(ma) => map (ma)(Left(_))
      case Right(mb) => map (mb)(Right(_))
    }

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = a => flatMap (f(a)) (g)

  def flatMap2[A,B](ma: M[A])(f: A => M[B]): M[B] = compose((_ : Unit) => ma, f)(())

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

  def flatMap3[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

  def compose2[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => join(map(f(a))(g))

}

object Monad {
  val optionMonad = new Monad[Option] {
    def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] = ma flatMap (f)

    def unit[A](a: => A): Option[A] = Some(a)
  }

  val listMonad = new Monad[List] {
    def flatMap[A, B](ma: List[A])(f: (A) => List[B]) = ma flatMap (f)

    def unit[A](a: => A) = List(a)
  }

  val streamMonad = new Monad[Stream] {
    def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]) = ma.flatMap (f)

    def unit[A](a: => A) = Stream(a)
  }

  def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
    def unit[A](a: => A) = p.succeed(a)

    def flatMap[A, B](pa: P[A])(f: (A) => P[B]) = p.flatMap(pa)(f)
  }

  import Chap6._

  def stateMonad[S] = new Monad[({type lambda[X] = State[S, X]})#lambda] {
    def flatMap[A, B](ma: (S) => (A, S))(f: (A) => (S) => (B, S)) = State.flatMap(ma)(f)

    def unit[A](a: => A): State[S, A] = (s: S) => (a, s)
  }
}