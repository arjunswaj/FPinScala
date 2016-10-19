import com.asb.snl.Stream

val stream = Stream(1, 2, 3, 4, 5)
stream.toList

stream.take(2)
stream.take(2).reverse()
stream.take(2).toList

stream.drop(2)
stream.drop(2).toList

stream.takeWhile(n => n < 3)
stream.takeWhile(n => n < 3).toList

stream.exists(p => p < 3)
stream.existByFolding(p => p < -3)