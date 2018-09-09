package nl.wwbakker.catssandbox.chapter10

import cats.data.NonEmptyList
import cats._
import cats.implicits._

case class CheckF[E, A](f : A => Either[E, A]) {
  def and(other : CheckF[E, A])(implicit s : Semigroup[E]) : CheckF[E, A] =
    CheckF{ a =>
      (f(a), other.f(a)) match {
        case (Left(e1), Left(e2)) => Left(s.combine(e1, e2))
        case (Left(e), Right(_)) => Left(e)
        case (Right(_), Left(e)) => Left(e)
        case (Right(v), _) => Right(v)
      }
    }
}

object Check {
  def apply[E, A](f : A => Either[E, A]) : Check[E, A] = Pure(f)
}

trait Check[E, A] {
  def check(value: A)(implicit s : Semigroup[E]): Either[E, A]

  def and(other : Check[E, A]) : Check[E, A] =
    And(this, other)

}
case class Pure[E, A](f : A => Either[E, A]) extends Check[E, A] {
  override def check(value: A)(implicit s : Semigroup[E]): Either[E, A] = f(value)
}
case class And[E, A](left : Check[E, A], right : Check[E, A]) extends Check[E, A] {
  override def check(value: A)(implicit s : Semigroup[E]): Either[E, A] =
    (left.check(value)(s), right.check(value)(s)) match {
      case (Left(e1), Left(e2)) => Left(s.combine(e1, e2))
      case (Left(e), Right(_)) => Left(e)
      case (Right(_), Left(e)) => Left(e)
      case (Right(v), _) => Right(v)
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