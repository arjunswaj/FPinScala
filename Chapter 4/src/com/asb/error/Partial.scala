package com.asb.error

/**
  * Partial to capture all the errors.
  * Created by arjun on 15/10/16.
  */
sealed trait Partial[+A, +B] {

  def map[AA >: A, C](f: B => C): Partial[AA, C] = this match {
    case Errors(e) => Errors(e)
    case Results(b) => Results(f(b))
  }

  def flatMap[AA >: A, C](f: B => Partial[AA, C]): Partial[AA, C] = this match {
    case Errors(e) => Errors(e)
    case Results(b) => f(b)
  }

  def orElse[AA >: A, C >: B](c: => Partial[AA, C]): Partial[AA, C] = this match {
    case Errors(e) => c
    case Results(b) => Results(b)
  }

  def map2[AA >: A, C, D](c: Partial[AA, C])(f: (B, C) => D): Partial[AA, D] = (this, c) match {
    case (Results(b1), Results(b2)) => Results(f(b1, b2))
    case (Results(b1), Errors(e2)) => Errors(e2)
    case (Errors(e1), Results(b2)) => Errors(e1)
    case (Errors(e1), Errors(e2)) => Errors(e1 ++ e2)
  }

}

case class Errors[+A](get: Seq[A]) extends Partial[A, Nothing]

case class Results[+B](get: B) extends Partial[Nothing, B]
