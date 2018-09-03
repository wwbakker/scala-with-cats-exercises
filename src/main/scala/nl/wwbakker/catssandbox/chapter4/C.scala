package nl.wwbakker.catssandbox.chapter4

import cats._
import cats.implicits._

object C {
  type ErrorOr[A] = Either[String, A]

  val monadError : MonadError[ErrorOr, String] = MonadError[ErrorOr, String]
  monadError.pure(42).map(_ + 5).ensure("number is too low")(_ > 50)

  Eval.later(5 + 5).memoize

}
