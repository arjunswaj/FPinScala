package com.asb.monoids

import scala.Predef.augmentString

sealed trait WC

case class Stub(chars: String) extends WC

case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }

    def zero: WC = Stub("")
  }

  import MonoidInstances._

  def count(s: String): Int = {
    def wc(c: Char): WC =
      if (Character.isWhitespace(c))
        Part("", 0, "")
      else
        Stub(c.toString)

    def unStub(s: String) = Math.min(s.length, 1)

    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(c) => unStub(c)
      case Part(l, w, r) => unStub(l) + w + unStub(r)
    }
  }
}