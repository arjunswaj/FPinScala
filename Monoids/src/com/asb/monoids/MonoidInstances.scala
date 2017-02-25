package com.asb.monoids

/**
  * Created by arjun on 25/02/17.
  */
object MonoidInstances {

  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2

    override def zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero = Nil
  }

}
