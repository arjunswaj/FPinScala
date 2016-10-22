import com.asb.snl.Stream

val stream = Stream(1, 2, 3, 4, 5, 6)
stream.toList

stream.take(2)
stream.take(2).reverse()
stream.take(2).toList

stream.drop(2)
stream.drop(2).toList

stream.takeWhile(n => n < 5)
stream.takeWhile(n => n < 5).toList

stream.exists(p => p < 3)
stream.existByFolding(p => p < -3)

stream.forAll(p => p <= 6)
stream.forAll(p => p < 2)

stream.takeWhileByFolding(n => n < 3)
stream.takeWhileByFolding(n => n < 4).toList

stream.headOptionByFolding
Stream().headOptionByFolding

stream.map(a => a + 5)
stream.map(a => a + 5).toList

stream.filter(a => 0 == a % 2)
stream.filter(a => 0 == a % 2).toList

val stream2 = Stream(10, 9, 8, 7, 6, 5)
stream.append(stream2)
stream.append(stream2).toList

stream.flatMap(t => Stream(t + 10, t + 20, t + 30))
stream.flatMap(t => Stream(t + 10, t + 20, t + 30)).toList

stream.find(p => 0 == p % 3)

val ones: Stream[Int] = Stream.cons(1, ones)
ones.take(5).toList

ones.exists(_ % 2 != 0)
ones.map(_ + 1).exists(_ % 2 == 0)
ones.forAll(_ != 1)