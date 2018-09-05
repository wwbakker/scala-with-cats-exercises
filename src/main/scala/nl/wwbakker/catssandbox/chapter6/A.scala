package nl.wwbakker.catssandbox.chapter6
import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._

object A {

  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = {
    for {
      a <- x
      b <- y
    } yield (a, b)
  }


}
