package com.asb.parsers

import java.util.regex.Pattern

import com.asb.error.Either
import com.asb.pbt
import com.asb.pbt.Gen

import scala.util.matching.Regex

/**
  * Parsers.
  * Created by arjun on 28/01/17.
  */
trait Parsers[Parser[+ _]] {
  self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def product[A, B](s1: Parser[A], s2: => Parser[B]): Parser[(A, B)] =
    flatMap(s1)(a => map(s2)(b => (a, b)))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)


  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))((a, b) => a :: b)

  def slice[A](p: Parser[A]): Parser[String]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(p1 => succeed(f(p1)))

  // Needed for Context Sensitive Grammars.
  def flatMap[A, B](a: Parser[A])(f: A => Parser[B]): Parser[B]

  def map2[A, B, C](a: Parser[A], b: => Parser[B])(f: (A, B) => C): Parser[C] =
    flatMap(a)(p1 => map(b)(p2 => f(p1, p2)))

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_, b) => b)

  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a, _) => a)

  def opt[A](p: Parser[A]): Parser[Option[A]] =
    p.map(Some(_)) or succeed(None)

  def whitespace: Parser[String] = "\\s*".r

  def digits: Parser[String] = "\\d+".r

  def thru(s: String): Parser[String] = (".*?" + Pattern.quote(s)).r

  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  def escapedQuoted: Parser[String] = token(quoted label "string literal")

  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  def double: Parser[Double] =
    doubleString map (_.toDouble) label "double literal"

  def token[A](p: Parser[A]): Parser[A] = attempt(p) <* whitespace

  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    sep1(p, p2) or succeed(List())

  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  def opL[A](p: Parser[A])(op: Parser[(A, A) => A]): Parser[A] =
    map2(p, many(op ** p))((h, t) => t.foldLeft(h)((a, b) => b._1(a, b._2)))

  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]): Parser[A] =
    start *> p <* stop

  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")


  def root[A](p: Parser[A]): Parser[A] =
    p <* eof

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def regex(r: Regex): Parser[String]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def token: Parser[A] = self.token(p)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def *>[B](p2: Parser[B]): Parser[B] = self.skipL(p, p2)

    def <*(p2: Parser[Any]): Parser[A] = self.skipR(p, p2)

    def sep(separator: Parser[Any]): Parser[List[A]] = self.sep(p, separator)

    def sep1(separator: Parser[Any]): Parser[List[A]] = self.sep1(p, separator)

    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

    def opL(op: Parser[(A, A) => A]): Parser[A] = self.opL(p)(op)

  }

  object Laws {

    import pbt.Prop._

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""

  def columnCaret = (" " * (col - 1)) + "^"
}

case class ParseError(stack: List[(Location, String)] = List()) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latest: Option[(Location, String)] =
    stack.lastOption

  def latestLoc: Option[Location] =
    latest map (_._1)

  /**
    * Display collapsed error stack - any adjacent stack elements with the
    * same location are combined on one line. For the bottommost error, we
    * display the full line, with a caret pointing to the column of the error.
    * Example:
    *1.1 file 'companies.json'; array
    *5.1 object
    *5.2 key-value
    *5.10 ':'
    * { "MSFT" ; 24,
    */
  override def toString =
    if (stack.isEmpty) "no error message"
    else {
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
          collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map { case (loc, msg) => loc.line.toString + "." + loc.col + " " + msg }.mkString("\n") +
        context
    }

  /* Builds a collapsed version of the given error stack -
   * messages at the same location have their messages merged,
   * separated by semicolons */
  def collapseStack(s: List[(Location, String)]): List[(Location, String)] =
    s.groupBy(_._1).
      mapValues(_.map(_._2).mkString("; ")).
      toList.sortBy(_._1.offset)

  def formatLoc(l: Location): String = l.line + "." + l.col
}

object Parsers {

}