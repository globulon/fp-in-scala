package com.promindis.fp

import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F] {
  F =>
  //map2 in terms of apply and unit
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(apply(unit(f.curried))(fa))(fb)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_.apply(_))

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  //apply in terms of map2
 def apply_[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa) (_.apply(_))

  //map from apply/unit
  def map_[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  //map from map2
  def map__[A, B](fa: F[A])(f: A => B): F[B] = map2(unit(f), fa) (_.apply(_))

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B])) { (a, fl) => map2(fl, f(a)) (_ :+ _) }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def factor[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def product[G[_]](G: Applicative[G]) = new Applicative[({type lambda[x] = (F[x], G[x])})#lambda] {
    type Product[X] = (F[X], G[X])

    override def map2[A, B, C](fa: Product[A], fb: Product[B])(f: (A, B) => C): Product[C] =
      (F.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))

    override def apply[A, B](fab: (F[(A) => B], G[(A) => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
      (F.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))


    def unit[A](a: => A): (F[A], G[A]) = (F.unit(a), G.unit(a))

    override def map[A, B](fa: (F[A], G[A]))(f: (A) => B): (F[B], G[B]) = (F.map(fa._1)(f), G.map(fa._2)(f))
  }
}


