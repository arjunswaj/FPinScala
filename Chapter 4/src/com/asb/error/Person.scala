package com.asb.error

case class Person(name: Name, age: Age) {
  override def toString: String = "{ Person : " + name.toString + ", " + age.toString + " }"
}

sealed class Name(val value: String) {
  override def toString: String = "{ Name : " + value + " }"
}

sealed class Age(val value: Int) {
  override def toString: String = "{ Age : " + value + " }"
}

object PersonBuilder {

  def mkName(name: String): Either[String, Name] =
    if ("" == name || null == name) Left("Name is empty")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of Range")
    else Right(new Age(age))

  def mkBetterName(name: String): Partial[String, Name] =
    if ("" == name || null == name) Errors(Seq("Name is empty"))
    else Results(new Name(name))

  def mkBetterAge(age: Int): Partial[String, Age] =
    if (age < 0) Errors(Seq("Age is out of Range"))
    else Results(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person)

  def mkBetterPerson(name: String, age: Int): Either[List[String], Person] =
    mkName(name).betterMap2(mkAge(age))(Person)

  def mkMuchBetterPerson(name: String, age: Int): Partial[String, Person] =
    mkBetterName(name).map2(mkBetterAge(age))(Person)
}
