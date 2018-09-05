package nl.wwbakker.catssandbox.chapter6

import cats.implicits._
import cats.data.{NonEmptyList, Validated}

object B {

  case class User(name: String, age: Int)


  type FormData = Map[String, String]
  type FailFast[A] = Either[NonEmptyList[String], A]
  type FailComplete[A] = Validated[NonEmptyList[String], A]

  val formValues: Map[String, String] = Map()

  def getValue(fieldName: String)(formData: FormData): FailFast[String] =
    formData.get(fieldName) match {
      case Some(value) => value.pure[FailFast]
      case None => s"'$fieldName' must be defined".invalidNel[String].toEither
    }

  def parseInt(s: String): FailFast[Int] =
  //    Try(s.toInt).toEither.swap.map(e => NonEmptyList[String](s"error parsing number, ${e.toString}", Nil)).swap
    Either.catchOnly[NumberFormatException](s.toInt)
      .leftMap(e => NonEmptyList[String](s"error parsing number, ${e.toString}", Nil))

  def nonBlank(fieldName: String)(s: String): FailFast[String] =
    s.pure[FailFast].ensure(NonEmptyList(s"$fieldName must not be blank", Nil))(_ != "")

  def nonNegative(fieldName: String)(i: Int): FailFast[Int] =
    i.pure[FailFast].ensure(NonEmptyList(s"$fieldName must be non-negative", Nil))(_ >= 0)


  def readName(formData: FormData): FailFast[String] =
    getValue("name")(formData)
      .flatMap(nonBlank("name"))

  def readAge(formData: FormData): FailFast[Int] =
    getValue("age")(formData)
      .flatMap(nonBlank("age"))
      .flatMap(parseInt)
      .flatMap(nonNegative("age"))

  def readUser(formData: FormData) : Validated[NonEmptyList[String], User] =
    (readName(formData).toValidated, readAge(formData).toValidated).mapN(User.apply)


  //  def readName(formValues : Map[String, String]) : ErrorsOr[String] =
  //    formValues.get("name") match {
  //      case None => List("name must be defined").invalid.toEither
  //      case Some(name) if name == "" => List("the name must not be blank").invalid.toEither
  //      case Some(name) => name.pure[ErrorsOr]
  //    }
  //
  //  def readAge(formValues : Map[String, String]) : ErrorsOr[Int] =
  //    formValues.get("age") match {
  //      case Some(value) => Try(value.toInt) match {
  //        case Failure(_) => List("age must be a valid integer").invalid.toEither
  //        case Success(i) if i < 0 => List("age must be non-")
  //        case Success(i) => i.pure[ErrorsOr]
  //      }
  //      case None => List("age must be defined").invalid.toEither
  //    }
}
