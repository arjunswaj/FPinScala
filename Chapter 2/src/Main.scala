/**
  * Created by arjun on 04/10/16.
  */
object Main {

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(tailRec.abs(6))
    println(tailRec.abs(-6))
    println(tailRec.fact(6))
    println(tailRec.fib(6))

    println(formatResult("Abs", 6, tailRec.abs))
    println(formatResult("Abs", -6, tailRec.abs))
    println(formatResult("Fact", 6, tailRec.fact))
    println(formatResult("Fib", 6, tailRec.fib))

    println(parametricPolymorphism.findFirstInString(Array("Arjun", "Shekar", "Bharadwaj"), "Bharadwaj"))
    println(parametricPolymorphism.findFirst[Int](Array(3, 4, 5), key => key == 4))

    val ascending = (o1: Int, o2: Int) => o1 < o2
    println(parametricPolymorphism.isSorted[Int](Array(3, 4, 5), ascending))
    println(parametricPolymorphism.isSorted[Int](Array(3), ascending))
    println(parametricPolymorphism.isSorted[Int](Array(3, 2, 1), ascending))

    val lol = partial.partial1[Int, Int, Int](5, (b, c) => b * c)
    println(lol(6))

    val adder = partial.curry[Int, Int, Int]((a, b) => a + b)
    println(adder(5)(6))

    println(partial.curry[Int, Int, Int]((a, b) => a * b)(73)(23))

    println(partial.uncurry(adder)(5, 6))

    println(partial.compose[Int, Int, Int](a => a + 5, b => b * 3)(5))
  }

}
