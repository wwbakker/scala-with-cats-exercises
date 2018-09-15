package nl.wwbakker.catssandbox.chapter10

import cats.data.NonEmptyList
import cats._
import cats.implicits._
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}

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

object Predicate {
  def apply[E, A](f : A => Validated[E, A]) : Predicate[E, A] = Pure(f)
}

trait Predicate[E, A] {
  def apply(value: A)(implicit s : Semigroup[E]): Validated[E, A]

  def and(other : Predicate[E, A]) : Predicate[E, A] =
    And(this, other)
}
case class Pure[E, A](f : A => Validated[E, A]) extends Predicate[E, A] {
  override def apply(value: A)(implicit s : Semigroup[E]): Validated[E, A] = f(value)
}
case class And[E, A](left : Predicate[E, A], right : Predicate[E, A]) extends Predicate[E, A] {
  implicit val takeFirstSemigroup : Semigroup[A] = new Semigroup[A] {
    override def combine(x: A, y: A): A = x
  }

  override def apply(value: A)(implicit s : Semigroup[E]): Validated[E, A] =
    left.apply(value).combine(right.apply(value))
}

case class Or[E, A](left : Predicate[E, A], right : Predicate[E, A]) extends Predicate[E, A] {
  implicit val takeFirstSemigroup : Semigroup[A] = new Semigroup[A] {
    override def combine(x: A, y: A): A = x
  }

  override def apply(value: A)(implicit s: Semigroup[E]): Validated[E, A] =
    (left.apply(value), right.apply(value)) match {
      case (Valid(x), _) => x.valid[E]
      case (_, Valid(x)) => x.valid[E]
      case (x, y) => x.combine(y)
    }
}

sealed trait Check[E, A, B] {
  def apply(a: A)(implicit s : Semigroup[E]): Validated[E, B]

  def map[C](func: A => C): Check[E, A, C] =
    CheckMap[E, A, B, C](this, func)

  def flatMap[C](func : A => Check[E, A, C]) : Check[E, A, C] =
    CheckFlatMap(this, func)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
    CheckAndThen[E, A, B, C](this, that)
}
object Check {
  def apply[E, A](predicate: Predicate[E, A]) = CheckPure(predicate)
}

case class CheckPure[E, A](predicate: Predicate[E, A]) extends Check[E, A, A] {
  override def apply(a: A)(implicit s : Semigroup[E]): Validated[E, A] = predicate(a)
}

case class CheckMap[E, A, B, C](check: Check[E, A, B], f : A => C) extends Check[E, A, C] {
  override def apply(a: A)(implicit s : Semigroup[E]): Validated[E, C] =
    check(a) match {
      case Valid(_) => Valid(f(a))
      case invalid @ Invalid(_) => invalid
    }
}
case class CheckFlatMap[E, A, B, C](check: Check[E, A, B], f : A => Check[E, A, C]) extends Check[E, A, C] {
  override def apply(a: A)(implicit s : Semigroup[E]): Validated[E, C] =
    check(a) match {
      case Valid(_) => f(a)(a)
      case invalid @ Invalid(_) => invalid
    }
}

case class CheckAndThen[E, A, B, C](check : Check[E, A, B], otherCheck : Check[E, B, C]) extends Check[E, A, C] {
  override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
    check(a) match {
      case Valid(v1) => otherCheck(v1)
      case e @ Invalid(_) => e
    }
}



object TestCheck {
  type PredicateNEL[A] = Predicate[NonEmptyList[String], A]

  type ErrorOr[A] = Validated[NonEmptyList[String], A]

  val valueNonEmpty : PredicateNEL[String] = Predicate[NonEmptyList[String], String](
      _.pure[ErrorOr].ensure(NonEmptyList("value is empty", Nil))(_.nonEmpty))

  val atLeast4Characters : PredicateNEL[String] = Predicate[NonEmptyList[String], String](
    _.pure[ErrorOr].ensure(NonEmptyList("value must be at least 4 characters", Nil))(_.length >= 4)
  )

  val isEven : PredicateNEL[Int] = Predicate[NonEmptyList[String], Int](
    _.pure[ErrorOr].ensure(NonEmptyList("value must be even", Nil))(_ % 2 == 0)
  )

  val both : PredicateNEL[String] = valueNonEmpty.and(atLeast4Characters)

  val val1: ErrorOr[String] = both("4341")

  val checkBoth : ErrorOr[Int] = Check(both).map(_.toInt).andThen(Check(isEven))("2001")
}