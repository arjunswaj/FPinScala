package com.asb.parsers

import com.asb.error.Either

/**
  * Parsers.
  * Created by arjun on 28/01/17.
  */
trait Parsers[ParseError, Parser[+ _]] {
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char]
}
