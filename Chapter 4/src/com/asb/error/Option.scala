package com.asb.error

/**
  * Created by arjun on 10/10/16.
  */
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def flatMap2[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  // So beautiful, never thunk of it
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def orElse2[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => this
  }

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) this else None)

  def filter2(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) this else None
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  val absO: Option[Double] => Option[Double] = lift(math.abs)

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = (age * 12.75) + (numberOfSpeedingTickets * 14.95)

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(va), Some(vb)) => Some(f(va, vb))
    case _ => None
  }

  def pureMap2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aVal => {
      b map (bVal => {
        f(aVal, bVal)
      })
    })

  def pureMap3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] =
    a flatMap (aVal => {
      b flatMap (bVal => {
        c map (cVal => {
          f(aVal, bVal, cVal)
        })
      })
    })

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge = Try(age.toInt)
    val optTickets = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(List())
    case x :: xs => x flatMap (xVal => sequence(xs) map (list => xVal :: list))
  }

  def sequence2[A](as: List[Option[A]]): Option[List[A]] =
    as.foldLeft[Option[List[A]]](Some(List()))((b, a) => a flatMap (aVal => b map (list => aVal :: list))) map (list => list.reverse)

  def sequence3[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(List()))((option, optionOfList) => pureMap2(option, optionOfList)((elem, list) => elem :: list))

  def parseInts(a: List[String]): Option[List[Int]] = sequence(a map (s => Try(s.toInt))) // Inefficient

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(List()))((a, b) => map2(f(a), b)((e, l) => e :: l))

  def sequence4[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(a => a)

  def map2For[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aVal <- a
      bVal <- b
    } yield f(aVal, bVal)

  def map3For[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] =
    for {
      aa <- a
      bb <- b
      cc <- c
    } yield f(aa, bb, cc)
}