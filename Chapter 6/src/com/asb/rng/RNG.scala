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

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(n => n - n % 2)
}
