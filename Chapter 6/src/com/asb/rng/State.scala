package com.asb.rng

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

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.reverse.foldLeft[State[S, List[A]]](unit(List()))((b, a) => a.map2(b)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

}
