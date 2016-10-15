import com.asb.error.{Either, Left, None, Option, Person, PersonBuilder, Right, Some, Swag}


Swag.mean(List(8.5, 3.2)) map (_.toString) getOrElse 5.0
Swag.mean(List()) map (_.toString) getOrElse "Hello"

Swag.variance(List())
Swag.variance(List(2, 3, 4, 5, 6))

Swag.sd(List(2, 3, 4, 5, 6))
Swag.sd2(List(2, 3, 4, 5, 6))

Swag.sd(List(-2, -3, -4, -5, -6))
Swag.sd2(List(2, 3, 4, 5, 6))

Option.sequence(List(Some(3), Some(4), None))
Option.sequence(List(Some(3), Some(4), Some(5)))

Option.sequence2(List(Some(3), Some(4), Some(5)))
Option.sequence2(List(Some(3), Some(4), None))

Option.sequence3(List(Some(3), Some(4), Some(5)))
Option.sequence3(List(Some(3), Some(4), None))

Option.traverse(List("3", "4", "5"))(s => Option.Try(s.toInt))
Option.sequence4(List(Some(3), Some(4), Some(5)))

Either.parseInsuranceRateQuote("12.45", "7")
Either.parseInsuranceRateQuote("12", "7")

Either.parseInsuranceRateQuoteUsingMap2("4", "9")

Either.sequence(List(Right(5), Right(4), Right(3)))
Either.sequence(List(Right(5), Left(new Exception("Screw You 2")), Left(new Exception("Screw You"))))

Either.sequenceLeft(List(Right(5), Right(4), Right(3)))
Either.sequenceLeft(List(Right(5), Left(new Exception("Screw You 2")), Left(new Exception("Screw You"))))

Either.traverse(List("3", "4", "5"))(s => Either.Try(s.toInt))
Either.traverse(List("3", "4.3", "5.5"))(s => Either.Try(s.toInt))

Either.sequenceAsTraverse(List(Right(5), Right(4), Right(3)))
Either.sequenceAsTraverse(List(Right(5), Left(new Exception("Screw You 2")), Left(new Exception("Screw You"))))

PersonBuilder.mkPerson("Arjun", 27)
PersonBuilder.mkPerson("Asdf", -27)

PersonBuilder.mkMuchBetterPerson("Arjun", 27)
PersonBuilder.mkMuchBetterPerson("", -27)
