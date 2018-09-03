package nl.wwbakker.catssandbox.chapter4

import cats.Monad
import cats.instances.option._ // for Monad
import cats.instances.list._   // for Monad
import cats.syntax.functor._ // for map
import cats.syntax.flatMap._ // for flatMap
import scala.language.higherKinds

object B {

  val opt1 = Monad[Option].pure(3)
  // opt1: Option[Int] = Some(3)

  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
  // opt2: Option[Int] = Some(5)

  val opt3 = Monad[Option].map(opt2)(a => 100 * a)
  // opt3: Option[Int] = Some(500)

  val list1 = Monad[List].pure(3)
  // list1: List[Int] = List(3)

  val list2 = Monad[List].
    flatMap(List(1, 2, 3))(a => List(a, a*10))
  // list2: List[Int] = List(1, 10, 2, 20, 3, 30)

  val list3 = Monad[List].map(list2)(a => a + 123)
  // list3: List[Int] = List(124, 133, 125, 143, 126, 153)

  def sumSquare[F[_]: Monad](a: F[Int], b : F[Int]) : F[Int] =
    //Monad[F].flatMap(a)(x => Monad[F].map(b)(y => x*x + y*y))

    //a.flatMap(x => b.map(y => x*x + y*y))

    for {
      x <- a
      y <- b
    } yield x*x + y*y

}
