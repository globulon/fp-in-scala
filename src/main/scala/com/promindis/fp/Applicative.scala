package com.promindis.fp

import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F] {
  //map2 in terms of apply and unit
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(apply(unit(f.curried))(fa))(fb)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def unit[A](a: => A): F[A]

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

}


