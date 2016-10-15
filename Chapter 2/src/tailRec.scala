import scala.annotation.tailrec

/**
  * Created by arjun on 04/10/16.
  */
object tailRec {

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def fib(n: Int): Int = {
    @tailrec
    def go(first: Int, second: Int, num: Int): Int = {
      if (num == n) first + second
      else go(second, first + second, num + 1)
    }
    if (n <= 0) 0
    else if (n == 1) 1
    else go(0, 1, 2)
  }


  def fact(n: Int): Int = {
    @tailrec
    def go(num: Int, acc: Int): Int = {
      if (num <= 0) acc
      else go(num - 1, num * acc)
    }

    go(n, 1)
  }

}
