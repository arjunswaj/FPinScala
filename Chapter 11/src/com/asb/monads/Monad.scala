package com.asb.monads

import com.asb.functors.Functor
import com.asb.rng.State

trait Monad[F[_]] extends Functor[F] {

  def unit[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))(map2(_, _)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((c, d) => map2(f(c), d)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    flatMap(ma)(a => unit(List.fill[A](n)(a)))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] =
    map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case x :: y => flatMap(f(x)) {
        n =>
          if (n)
            map(filterM(y)(f))(x :: _)
          else
            filterM(y)(f)
      }
    }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(b => map(g(b))(c => c))

  def flatMapAsCompose[A, B](fa: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => fa, f)(())

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(k => k)

  def flatMapAsJoin[A, B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(k => f(k)))

  def composeAsJoin[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(b => map(g(b))(c => c)))

}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
  val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)

    def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = fa flatMap f
  }
}

object IntStateMonad extends Monad[({type IntState[A] = State[Int, A]})#IntState] {
  def unit[A](a: => A): State[Int, A] = State(s => (a, s))

  def flatMap[A, B](fa: State[Int, A])(f: (A) => State[Int, B]): State[Int, B] = fa flatMap f
}

object monads {
  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))

    def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] = fa flatMap f
  }

  val stringStateMonad = stateMonad[String]

  val replicate = stringStateMonad.replicateM(5, State.unit("Hello"))
  val map2 = stringStateMonad.map2(State.unit(7), State.unit(9.25))((a, b) => a * b)
  val sequence = stringStateMonad.sequence(List.fill(10)(State.unit("Lol")))

  val F = stateMonad[Int]

  def getState[S]: State[S, S] = State(s => (s, s))

  def setState[S](s: => S): State[S, Unit] = State(_ => ((), s))
}