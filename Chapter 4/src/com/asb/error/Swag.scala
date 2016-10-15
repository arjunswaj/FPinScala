package com.asb.error

/**
  * Created by arjun on 10/10/16.
  */
object Swag {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs map (x => Math.pow(x - m, 2))))

  def deviation(x: Double, m: Double): Double = x - m

  def squared(x: Double): Double = Math.pow(x, 2)

  def varianceVerbose(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs map (x => squared(deviation(x, m)))))

  def sd(xs: Seq[Double]): Option[Double] = variance(xs) map (x => Math.sqrt(x))

  def squareRoot(x: Double): Option[Double] =
    if (x >= 0) Some(Math.sqrt(x))
    else None

  def sd2(xs: Seq[Double]): Option[Double] = variance(xs) flatMap (x => squareRoot(x))

}
