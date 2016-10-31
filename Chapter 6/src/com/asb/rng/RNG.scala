package com.asb.rng

/**
  * RNG.
  * Created by arjun on 29/10/16.
  */
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

object random {

  def sameRandomPair(rng: RNG): (Int, Int) = {
    val (i1, _) = rng.nextInt
    val (i2, _) = rng.nextInt
    (i1, i2)
  }

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (i, rng2) if i < 0 => (-(i + 1), rng2)
      case (i, rng2) => (i, rng2)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val d = i + double(rng2)._1
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (a, b) = intDouble(rng)
    (a.swap, b)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (a, r1) = doubleInt(rng)
    val (b, r2) = doubleInt(r1)
    val (c, r3) = doubleInt(r2)
    ((a._1, b._1, c._1), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(n: Int, acc: List[Int], rng: RNG): (List[Int], RNG) =
      if (0 >= n)
        (acc, rng)
      else {
        val ni = rng.nextInt
        loop(n - 1, ni._1 :: acc, ni._2)
      }

    val (list, r) = loop(count, List(), rng)
    (list.reverse, r)
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[S, A, B](s: S => (A, S))(f: A => B): S => (B, S) =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    mapAsFlatMap(nonNegativeInt)(n => n - n % 2)

  def doubleElegant(rng: RNG): (Double, RNG) =
    mapAsFlatMap(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, r1) = ra(rng)
      val (b, r2) = rb(r1)
      (f(a, b), r2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      val (list, tx) = fs.foldLeft[(List[A], RNG)]((List(), rng))((a, b) => {
        val (m, r1) = b(a._2)
        (m :: a._1, r1)
      })
      (list.reverse, tx)
    }

  // This will be reverse, mind it. This is the book solution.
  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
  fs.foldRight(unit(List[A]()))((acc, t) => map2AsFlatMap(acc, t)(_ :: _))

  def intsAsSeq(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence[Int](List.fill(count)(x => x.nextInt))(rng)

  def intsAsSeq2(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence2[Int](List.fill(count)(x => x.nextInt))(rng)

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    rng =>
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        (mod, rng2)
      else nonNegativeLessThan(n)(rng2) // Note that there is an errata in the book and they use rng instead of rng2
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng =>
      val (a, b) = f(rng)
      g(a)(b)
  }

  def nonNegativeLessThanByFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThanByFlatMap(n)
    })

  def mapAsFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2AsFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => {
      flatMap(rb)(b => unit(f(a, b)))
    })

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}

import State._

case class State[S, +A](run: S => (A, S)) {
  def mapFirstPrinciples[B](f: A => B): State[S, B] =
    State(s => (f(run(s)._1), s))

  def map2FirstPrinciples[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = rb.run(s2)
      (f(a, b), s3)
    })

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      g(a).run(s2)
    })

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  // Note: You can use Unit inside flatMap inside flatMap
  // or just Map inside flatMap

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => rb.map(b => f(a, b)))

  // Or use For comprehension

  def map2ForComprehension[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    for {
      a <- this
      b <- rb
    } yield f(a, b)

}

object State {
  type Rand[A] = State[RNG, A]

  def int: Rand[Int] = State(rng => rng.nextInt)

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.reverse.foldLeft[Rand[List[A]]](unit(List()))((b, a) => a.map2(b)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

}