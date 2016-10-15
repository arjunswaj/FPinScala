import scala.annotation.tailrec

/**
  * Created by arjun on 05/10/16.
  */
object parametricPolymorphism {

  def findFirstInString(as: Array[String], key: String): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (as(n) == key) n
      else loop(n + 1)
    }
    loop(0)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (ordered(as(n - 1), as(n))) loop(n + 1)
      else false
    }

    if (as.length <= 1) true
    else loop(1)
  }

}

