package nl.wwbakker.catssandbox.chapter4

import scala.language.higherKinds
import cats.Functor
import cats.instances.function._
import cats.syntax.functor._

object A {
  trait Monad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    def map[A, B](value: F[A])(func: A => B): F[B] =
    // intellij snapt het niet, maar het klopt wel
      flatMap(value)(func.map(pure))
  }
}
