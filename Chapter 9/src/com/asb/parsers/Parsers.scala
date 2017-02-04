package com.asb.parsers

import com.asb.error.Either
import com.asb.pbt
import com.asb.pbt.Gen

/**
  * Parsers.
  * Created by arjun on 28/01/17.
  */
trait Parsers[ParseError, Parser[+ _]] {
  self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def product[A, B](s1: Parser[A], s2: Parser[B]): Parser[(A, B)]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))((a, b) => a :: b)

  def slice[A](p: Parser[A]): Parser[String]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def map2[A, B, C](a: Parser[A], b: Parser[B])(f: (A, B) => C): Parser[C] =
    this.product(a, b).map(k => f(k._1, k._2))

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def slice: Parser[String] = self.slice(p)
  }

  object Laws {

    import pbt.Prop._

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }

}
