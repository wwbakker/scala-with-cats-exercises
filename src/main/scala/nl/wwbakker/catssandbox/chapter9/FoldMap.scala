package nl.wwbakker.catssandbox.chapter9

import cats.Monoid
import cats.syntax.monoid._

object FoldMap {
  def foldMap[A, B : Monoid](seq : Vector[A])(f : A => B) : B =
    seq.foldLeft(Monoid.empty[B])(_ |+| f(_))
}
