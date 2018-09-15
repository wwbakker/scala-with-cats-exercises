package nl.wwbakker.catssandbox.chapter11

import cats.Monoid

trait BoundedSemiLattice[A] extends Monoid[A] {
  def combine(a1: A, a2: A): A
  def empty: A
}


object BoundedSemiLattice {
  implicit val Ints: BoundedSemiLattice[Int] =
    new BoundedSemiLattice[Int] {
      override def combine(a1: Int, a2: Int): Int = a1 max a2
      override def empty: Int = 0
    }

  implicit def Sets[A]: BoundedSemiLattice[Set[A]] =
    new BoundedSemiLattice[Set[A]] {
      override def combine(a1: Set[A], a2: Set[A]): Set[A] =
        a1 ++ a2
      override def empty: Set[A] = Set.empty[A]
    }
}