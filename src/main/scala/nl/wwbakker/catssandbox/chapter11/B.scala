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

  trait GCounter[F[_, _], K, V] {
    def increment(f: F[K, V])(k: K, v: V)
                 (implicit m: Sum[V]): F[K, V]

    def merge(f1: F[K, V], f2: F[K, V])
             (implicit b: BoundedSemiLattice[V]): F[K, V]

    def total(f: F[K, V])
             (implicit m: Sum[V]): V
  }

  object GCounter {
    def apply[F[_,_], K, V]
    (implicit counter: GCounter[F, K, V]) =
      counter
  }

  implicit def MapGCounter[K, V] : GCounter[Map, K, V] = new GCounter[Map, K, V] {
    override def increment(f: Map[K, V])(k: K, v: V)(implicit m: Sum[V]): Map[K, V] = {
      val newValue = f.getOrElse(k, m.empty) |+| v
      f.updated(k, newValue)
    }

    override def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
      f1 |+| f2

    override def total(f: Map[K, V])(implicit m: Sum[V]): V =
      f.values.toList.combineAll
  }

//  final case class GCounter[A](counters: Map[String, A]) {
//    def increment(machine: String, amount: A)(implicit m : Sum[A]): GCounter[A] = {
//      val v = counters.getOrElse(machine, m.empty) |+| amount
//      GCounter(counters.updated(machine, v))
//    }
//
//
//
//    def merge(that: GCounter[A])(implicit m : Max[A]): GCounter[A] =
//      GCounter[A](this.counters |+| that.counters)
//
//    def total(implicit m : Sum[A]): A =
//      counters.values.toList.combineAll
//  }
}

