package com.promindis.fp

import annotation.tailrec

object Chap6 {
  type State[S,+A] = S => (A,S)

  type Rand[+A] = State[RNG, A]

  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    def simple(seed: Long): RNG = new RNG {
      def nextInt = {
        val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
      }
    }

    val positiveInt: Rand[Int] = (rng: RNG)=> {
      val (i, rng2) = rng.nextInt
      if (i == Int.MinValue)  positiveInt(rng2)
      else (math.abs(i), rng2)
    }

    val double: Rand[Double] = (rng: RNG) => {
      val (i, rng2) = positiveInt(rng)
      (i / (Int.MaxValue.toDouble + 1), rng2)
    }

    def range(i: Double, j: Double): Rand[Double] =
      rng => double(rng) match {
        case  (d, rng2) => ((i + (j - i) * d), rng2)
      }

    val boolean: State[RNG,Boolean] = rng => {
      val (i, rng2) = rng.nextInt
      (i % 2 == 0, rng2)
    }

    //can use map too
    def range(start: Int, stopExclusive: Int): State[RNG, Int] =
      rng => rng.nextInt match {
        case (i, rng2) => (start + (i % (stopExclusive - start)), rng2)
      }

    def intDouble(rng: RNG): ((Int,Double), RNG) = {
      val (i, rng2) = positiveInt(rng)
      val (d, rng3) = double(rng2)
      ((i, d), rng3)
    }
    def doubleInt(rng: RNG): ((Double,Int), RNG) = {
      val ((i, d), rng2) = intDouble(rng)
      ((d, i), rng2)
    }
    def double3(rng: RNG): ((Double,Double,Double), RNG) = {
      val (d, rng2) = double(rng)
      val (d2, rng3) = double(rng2)
      val (d3, rng4) = double(rng3)
      ((d, d2, d3), rng4)
    }

    def ints(count: Int): Rand[List[Int]] = (rng: RNG) => {
      @tailrec
      def iter(acc: List[Int], n: Int, irng: RNG): (List[Int], RNG) =
        if (n == 0) (acc, irng)
        else {
          val (i, rng2) = positiveInt(irng)
          iter(i::acc, n - 1, rng2)
        }

      iter(Nil, count, rng)
    }

    def unit[A](a: A): Rand[A] =  rng => (a, rng)

    val int: Rand[Int] = _.nextInt


    def flatMap[A, B](rand: Rand[A])(f: A => Rand[B]): Rand[B] =
      (rng: RNG) => {
        val (i, rng2) = rand(rng)
        f(i).apply(rng2)
      }

//    def map[A, B](rand: Rand[A])(f: A => B): Rand[B] =
//      (rng: RNG) => {
//        val (a, rng2) = rand(rng)
//        (f(a), rng2)
//      }

    def map[A, B](rand: Rand[A])(f: A => B): Rand[B] =
      flatMap(rand) { i => unit(f(i)) }


    //    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
//      (rng: RNG) => {
//        val (a, rng1) = ra(rng)
//        val (b, rng2) = rb(rng1)
//        (f(a, b), rng2)
//      }

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        flatMap(ra) { i =>
          map(rb) { j =>
            f(i, j)
          }
        }

    def positiveMax(n: Int): Rand[Int] = map(positiveInt) { i => (i / (Integer.MAX_VALUE / n)) }

    def double_1: Rand[Double] = map(positiveInt) (_ / (Int.MaxValue.toDouble + 1))


    def intDouble_1: Rand[(Int, Double)] = map2(positiveInt, double) ((_, _))

    def doubleInt_1: Rand[(Double, Int)] = map2(double, positiveInt)((_, _))

    def traverse[A, B](rs: List[A])(f: A => Rand[B]): Rand[List[B]] =
      rs.foldLeft(unit[List[B]](Nil)) { (rl, a) =>
        map2(f(a), rl ) (_::_)
      }

    def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = traverse(rs)(identity)

    def ints_1(n: Int): Rand[List[Int]] = sequence(List.fill(n)(positiveInt))
  }

  object State {
    def unit[S, A](a: A) = (s: S) => (a, s)

    def flatMap[S, A, B](action: State[S, A])(f: A => State[S, B]): State[S, B] =
      (s: S) => {
        val (a, s2) = action.apply(s)
        f(a).apply(s2)
      }

    def map[S, A, B](action: State[S, A])(f: A => B): State[S, B] =
      flatMap(action) { a => unit(f(a)) }

    def map2[S, A, B, C](act: State[S, A], act2: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(act) { a =>
        map(act2) { b =>  f(a, b) }
      }

    def traverse[S, A, B] (as: List[A])(f: A => State[S, B]): State[S, List[B]] =
      as.reverse.foldLeft(unit[S, List[B]](Nil)) { (bs, a) =>
        map2(f(a), bs) (_::_)
      }

    def sequence[S,A](as: List[State[S, A]]): State[S, List[A]] = traverse(as)(identity)

    def get[S]: State[S, S] = (s: S)  => (s, s)

    def set[S](s: S): State[S, Unit] = (s0: S) => ((), s)

    def modify[S](f: S => S): State[S, Unit] =
      flatMap(get[S]) { s =>
        map(set(f(s))) { _ => ()}
      }

    implicit def toRichState[S, A](s: State[S, A]) = new {
      def map[B](f: A => B) = State.map(s)(f)

      def flatMap[B](f: A => State[S, B]) = State.flatMap(s)(f)
    }

    def apply[S, A](f: S => (A,S)): State[S, A] = f
  }


  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object Machine {
    import State._

    def action(input: Input): State[Machine, Int] = {
      case m @ Machine(_, 0, _) => (m.coins, m)
      case Machine(true, candies, coins) if (input == Coin) && (candies > 0) => (coins + 1, Machine(locked = false, candies, coins + 1))
      case m @ Machine(true, _, _) if (input == Turn) => (m.coins, m)
      case Machine(false, candies, coins) if (input == Turn) => (coins, Machine(locked = true, candies - 1, coins))
      case Machine(false, candies, coins) if (input == Coin) => (coins + 1, Machine(locked = false, candies, coins + 1))
    }

    def simulateMachine(inputs: List[Input]): State[Machine, Int] =
      map(traverse(inputs)(action(_))) { coins => coins.last }
  }

  //book example
  val sim_1 = Machine.simulateMachine(List(Coin, Coin, Coin, Coin)).apply(Machine(locked = false, 17, 10))
  //can't accept coins when no more candies
  val sim_2 = Machine.simulateMachine(List(Coin, Coin)).apply(Machine(locked = true, candies = 0, 10))

  val sim_3 = Machine.simulateMachine(List(Coin, Turn, Coin, Turn)).apply(Machine(locked = true, 7, 5))
}

