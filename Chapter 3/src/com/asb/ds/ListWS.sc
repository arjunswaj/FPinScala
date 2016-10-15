import com.asb.ds.{Cons, List, Nil}

List.sum(List(2, 3, 4))

List.x

List.apply(2, 3, 4)
List(2, 3, 4)

val a = List(1, 2, 3, 4, 5)
val b = List(6, 7, 8, 9, 10)
List.tail(a)
// List.tail(List())

List.tail(List("Arjun", "LOL", "Haha"))
List.setHead(a, 999)
a

List.setHead(Nil, 18570)

List.drop(a, 3)
List.drop(a, 5)
// List.drop(a, 7)

List.dropWhile(a, (n: Int) => n <= 2)
List.dropWhile(a, (n: Int) => n <= 20)

// Note that this is NOT a filter, it's dropWhile. Duh.
List.dropWhile(a, (n: Int) => 0 == n % 2)

List.append(a, b)
a

List.init(a)
a
List.init(b)

List.dropWhile2(a)(n => n < 3)

List.sum2(List(2, 3, 4))

// Copier!!!
List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))

List.length(a)

List.sum3(List(2, 3, 4))
List.prod3(List(2, 3, 4))

List.reverse(a)

val c = List.appendRight(a, b)
List.appendLeft(a, b)

List.flattenRight(List(List(1, 2, 3), List(4, 5, 6, 7)))
List.flattenLeft(List(List(1, 2, 3), List(4, 5), List(6, 7)))

List.addOneToAllElements(List(1, 2))

List.convertDoubleToString(List(2.5, 5.3, 9.8))

List.map(List("Arjun", "Shekar", "Bharadwaj"))(a => a.length)

List.filter(a)(a => 0 == a % 2)

List.flatMap(List(List(1, 2, 3), List(4, 5), List(6, 7)))(a => a)
List.flatMap(List(1, 2, 3))(i => List(i, i))

List.filterFromFlatMap(a)(a => 0 == a % 2)

List.summer(a, b)
List.zipWith(a, b)(_ + _)

List.take(a, 3)

List.takeWhile[Int](a, k => k <= 3)

List.splitAt(3)(a)

List.reverser(2)(c)

List.forAll(c)(a => a < 11)

List.exists(c)(a => a < 2)

List.scanLeft(c, 0)(_ + _)

List.scanRight(c, 0)(_ + _)

List.hasSubSequence(c, List(1, 4, 9, 100))

List.hasContiguousSequence(c, List(2, 3, 4, 5))
List.hasContiguousSequence(c, List(2, 4, 5))