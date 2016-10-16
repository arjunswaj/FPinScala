package com.asb.snl

/**
  * Created by arjun on 16/10/16.
  */
object Main {

  def main(args: Array[String]): Unit = {
    val a = 45
    laziness.if2(a > 22, () => println("a"), () => println("b"))
    laziness.if3(a < 22, println("a"), println("b"))
    laziness.maybeTwice(true, {println("Lulz"); 1 + 41})
    laziness.maybeTwice2(true, {println("Lolol"); 1 + 41})
  }

}
