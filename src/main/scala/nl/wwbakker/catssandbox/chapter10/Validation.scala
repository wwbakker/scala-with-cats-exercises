package nl.wwbakker.catssandbox.chapter10

import cats.data.NonEmptyList
import cats._
import cats.implicits._

//trait Check[E, A] {
//  self =>
//
//  def apply(value: A): Either[E, A]
//
//  def and(that: Check[E, A])(implicit andS : Semigroup[CheckA[E]]) : Check[E, A] =
//    andS.combine(self, that)
//
//}

case class Check[E, A](f : A => Either[E, A]) {
  def and(other : Check[E, A])(implicit s : Semigroup[E]) : Check[E, A] =
    Check{ a =>
      (f(a), other.f(a)) match {
        case (Left(e1), Left(e2)) => Left(s.combine(e1, e2))
        case (Left(e), Right(_)) => Left(e)
        case (Right(_), Left(e)) => Left(e)
        case (Right(v), _) => Right(v)
      }
    }
}

object TestCheck {
  type CheckEL[A] = Check[NonEmptyList[String], A]

  type ErrorOr[A] = Either[NonEmptyList[String], A]

  val valueNonEmpty : CheckEL[String] = Check[NonEmptyList[String], String](
      _.pure[ErrorOr].ensure(NonEmptyList("value is empty", Nil))(_.nonEmpty))

  val atLeast4Characters : CheckEL[String] = Check[NonEmptyList[String], String](
    _.pure[ErrorOr].ensure(NonEmptyList("value must be at least 4 characters", Nil))(_.length >= 4)
  )

  val both : CheckEL[String] = valueNonEmpty.and(atLeast4Characters)
}