package com.asb.functors

object FunctorInstances {

  val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f
  }

}
