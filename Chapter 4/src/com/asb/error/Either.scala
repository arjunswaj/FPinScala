package com.asb.error

/**
  * Created by arjun on 12/10/16.
  */
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)

  def betterMap2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[List[E], C] = this match {
    case Left(e) => b match {
      case Left(ee: E) => Left(List(e, ee))
      case _ => Left(List(e))
    }
    case Right(aa) => b match {
      case Left(ee: E) => Left(List(ee))
      case Right(bb) => Right(f(aa, bb))
    }
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = (age * 3.75) + (numberOfSpeedingTickets * 4.95)

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] =
    for {
      a <- Try(age.toInt)
      n <- Try(numberOfSpeedingTickets.toInt)
    } yield insuranceRateQuote(a, n)

  def parseInsuranceRateQuoteUsingMap2(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] =
    Try(age.toInt).map2(Try(numberOfSpeedingTickets.toInt))(insuranceRateQuote)

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight[Either[E, List[A]]](Right(List()))((either, eitherOfList) => either flatMap (a => eitherOfList map (list => a :: list)))

  // Buggy if Error occurs
  def sequenceLeft[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
  es.foldLeft[Either[E, List[A]]](Right(List()))((eitherOfList, either) => either flatMap (a => eitherOfList map (list => a :: list))) map (list => list.reverse)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(List()))((a, eitherOfList) => f(a) flatMap (b => eitherOfList map (list => b :: list)))

  def sequenceAsTraverse[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(a => a)


}