import com.asb.snl.Stream

val stream = Stream(1, 2, 3, 4, 5)
stream.toList

stream.take(2)
stream.take(2).reverse()
stream.take(2).toList