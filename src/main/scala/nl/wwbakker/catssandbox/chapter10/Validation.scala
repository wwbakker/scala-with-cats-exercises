package nl.wwbakker.catssandbox.chapter10

import cats.data.NonEmptyList
import cats._
import cats.implicits._
import cats.data.Validated
import cats.data.Validated.Valid

//case class CheckF[E, A](f : A => Either[E, A]) {
//  def and(other : CheckF[E, A])(implicit s : Semigroup[E]) : CheckF[E, A] =
//    CheckF{ a =>
//      (f(a), other.f(a)) match {
//        case (Left(e1), Left(e2)) => Left(s.combine(e1, e2))
//        case (Left(e), Right(_)) => Left(e)
//        case (Right(_), Left(e)) => Left(e)
//        case (Right(v), _) => Right(v)
//      }
//    }
//}
//

object Check {
  def apply[E, A](f : A => Validated[E, A]) : Check[E, A] = Pure(f)
}

trait Check[E, A] {
  def check(value: A)(implicit s : Semigroup[E]): Validated[E, A]

  def and(other : Check[E, A]) : Check[E, A] =
    And(this, other)

}
case class Pure[E, A](f : A => Validated[E, A]) extends Check[E, A] {
  override def check(value: A)(implicit s : Semigroup[E]): Validated[E, A] = f(value)
}
case class And[E, A](left : Check[E, A], right : Check[E, A]) extends Check[E, A] {
  implicit val takeFirstSemigroup : Semigroup[A] = new Semigroup[A] {
    override def combine(x: A, y: A): A = x
  }

  override def check(value: A)(implicit s : Semigroup[E]): Validated[E, A] =
    left.check(value).combine(right.check(value))
}

case class Or[E, A](left : Check[E, A], right : Check[E, A]) extends Check[E, A] {
  implicit val takeFirstSemigroup : Semigroup[A] = new Semigroup[A] {
    override def combine(x: A, y: A): A = x
  }

  override def check(value: A)(implicit s: Semigroup[E]): Validated[E, A] =
    (left.check(value), right.check(value)) match {
      case (Valid(x), _) => x.valid[E]
      case (_, Valid(x)) => x.valid[E]
      case (x, y) => x.combine(y)
    }
}

object TestCheck {
  type CheckEL[A] = Check[NonEmptyList[String], A]

  type ErrorOr[A] = Validated[NonEmptyList[String], A]

  val valueNonEmpty : CheckEL[String] = Check[NonEmptyList[String], String](
      _.pure[ErrorOr].ensure(NonEmptyList("value is empty", Nil))(_.nonEmpty))

  val atLeast4Characters : CheckEL[String] = Check[NonEmptyList[String], String](
    _.pure[ErrorOr].ensure(NonEmptyList("value must be at least 4 characters", Nil))(_.length >= 4)
  )

  val both : CheckEL[String] = valueNonEmpty.and(atLeast4Characters)
}