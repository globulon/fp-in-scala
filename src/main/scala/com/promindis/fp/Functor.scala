package com.promindis.fp
import scala.language.higherKinds
import scala.language.reflectiveCalls

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](f: F[(A, B)]): (F[A], F[B]) = (map(f)(_._1), map(f)(_._2))
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa.map(f)
  }
}

trait Monad[M[_]] extends Applicative[M] {
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))

  def cofactor[A,B](e: Either[M[A], M[B]]): M[Either[A, B]] =
    e match {
      case Left(ma) => map (ma)(Left(_))
      case Right(mb) => map (mb)(Right(_))
    }

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = a => flatMap (f(a)) (g)

  def flatMap2[A,B](ma: M[A])(f: A => M[B]): M[B] = compose((_ : Unit) => ma, f)(())

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

  def flatMap3[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))

  def compose2[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = a => join(map(f(a))(g))

  override def apply[A, B](mf: M[A => B])(ma: M[A]): M[B] = flatMap(mf) (map(ma)(_))

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
  import State._

  def stateMonad[S] = new Monad[({type lambda[X] = State[S, X]})#lambda] {
    def flatMap[A, B](ma: (S) => (A, S))(f: (A) => (S) => (B, S)) = State.flatMap(ma)(f)

    def unit[A](a: => A): State[S, A] = (s: S) => (a, s)
  }

  def push(x: Int): State[List[Int], Int] = l => (x, x::l)

  val pop: State[List[Int], Unit] = {
    case h::t => ((), t)
    case Nil => ((), Nil)
  }

  val testState: State[List[Int], Unit] = for {
    _ <- push(3)
    _ <- push(5)
    _ <- push(7)
    r <- pop
  } yield r

  val stackMonad = stateMonad[List[Int]]

  //applied to List give List(4,4,4,4)
  val replicateModifyState = stackMonad.replicateM(4, push(4))

  //operate on result, and keep the state
  val map2State = stackMonad.map2(push(3), push(4))((_, _))

  //collect side effects in the list and get the list state anyway
  val sequenceState = stackMonad.sequence(List(push(3), push(4), push(5)))

  def getState[S]: State[S, S] = s => (s, s)
  def setState[S](f:  => S): State[S, Unit]  = s => ((), s)

  val M = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int,A)] =
    as.foldLeft(M.unit(List[(Int, A)]()))((acc,a) => for {
      n <- getState
      xs <- acc
      _ <- setState(n + 1)
    } yield ((n, a) :: xs)).apply(0)._1.reverse
}