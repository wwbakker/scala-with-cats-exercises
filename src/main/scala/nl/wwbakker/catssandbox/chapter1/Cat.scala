package nl.wwbakker.catssandbox.chapter1

import cats.{Eq, Show}
import cats.instances.all._
import cats.syntax.all._

final case class Cat(name : String, age : Int, color : String)

object Cat {
  implicit val catShowable : Show[Cat] =
    Show.show(a => s"${a.name.show} is a ${a.age.show} year-old ${a.color.show} cat.")

  implicit val catEqv : Eq[Cat] =
    Eq.instance(_ == _)
}