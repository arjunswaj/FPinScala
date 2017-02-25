package com.asb.monoids

import com.asb.pbt.Gen
import com.asb.pbt.Prop._

/**
  * Created by arjun on 25/02/17.
  */
object MonoidLaws {


  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
  // Associativity
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z))(p => m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3)) &&
      // Identity
      forAll(gen)((a: A) => m.op(a, m.zero) == a && m.op(m.zero, a) == a)

}
