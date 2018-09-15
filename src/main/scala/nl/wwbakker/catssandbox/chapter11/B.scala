package nl.wwbakker.catssandbox.chapter11

import cats.Monoid
import cats.instances.list._   // for Monoid
import cats.instances.map._    // for Monoid
import cats.syntax.semigroup._ // for |+|
import cats.syntax.foldable._  // for combineAll

object B {

  trait BoundedSemiLattice[A] extends Monoid[A] {
    def combine(a1: A, a2: A): A
    def empty: A
  }

  trait Max[A] extends BoundedSemiLattice[A]
  trait Sum[A] extends BoundedSemiLattice[A]

  implicit val IntsMerge: Max[Int] =
    new Max[Int] {
      override def combine(a1: Int, a2: Int): Int = a1 max a2
      override def empty: Int = 0
    }
  implicit val IntsSum: Sum[Int] =
    new Sum[Int] {
      override def combine(a1: Int, a2: Int): Int = a1 + a2
      override def empty: Int = 0
    }

  implicit def Sets[A]: BoundedSemiLattice[Set[A]] =
    new BoundedSemiLattice[Set[A]] {
      override def combine(a1: Set[A], a2: Set[A]): Set[A] =
        a1 ++ a2
      override def empty: Set[A] = Set.empty[A]
    }


  final case class GCounter[A](counters: Map[String, A]) {
    def increment(machine: String, amount: A)(implicit m : Sum[A]): GCounter[A] = {
      val v = counters.getOrElse(machine, m.empty) |+| amount
      GCounter(counters.updated(machine, v))
    }



    def merge(that: GCounter[A])(implicit m : Max[A]): GCounter[A] =
      GCounter[A](this.counters |+| that.counters)

    def total(implicit m : Sum[A]): A =
      counters.values.toList.combineAll
  }
}

