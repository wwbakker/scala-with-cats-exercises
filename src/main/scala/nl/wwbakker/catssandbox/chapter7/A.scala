package nl.wwbakker.catssandbox.chapter7

import nl.wwbakker.catssandbox.chapter2.BooleanMonoids.Monoid

//scala> List(1, 2, 3).foldLeft(List[Int]())((acc, i) => i :: acc)
//res6: List[Int] = List(3, 2, 1)

//scala> List(1, 2, 3).foldRight(List[Int]())((acc, i) => acc :: i)
//res8: List[Int] = List(1, 2, 3)

object A {
  def map[A, B](list : List[A])(fn : A => B) : List[B] =
    list.foldRight(List.empty[B])((a, acc) => fn(a) :: acc)

  def flatMap[A, B](list : List[A])(fn : A => List[B]) : List[B] =
    list.foldRight(List.empty[B])((a, acc) => fn(a) ::: acc)

  def filter[A](list : List[A])(fn : A => Boolean) : List[A] =
    list.foldRight(List.empty[A])((a, acc) => if(fn(a)) a :: acc else acc)

  def sum[A : Monoid](list : List[A]) : A =
    list.foldRight(Monoid[A].empty)(Monoid[A].combine)

  implicit val sumMonoid : Monoid[Int] = new Monoid[Int] {
    override def empty: Int = 0
    override def combine(x: Int, y: Int): Int = x + y
  }

}