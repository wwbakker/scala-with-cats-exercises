package nl.wwbakker.catssandbox.chapter1

import cats.Eq
import cats.syntax.eq._
import cats.instances.option._

import Cat._

object Worksheet2 {
  def run(): Unit = {
    val cat1 = Cat("Jan", 4, "blue")
    val cat2 = Cat("Kees", 4, "blue")

    val optionCat1 : Option[Cat] = Option(cat1)
    val optionCat2 : Option[Cat] = Option.empty[Cat]
    //    cat.show
    println(cat1 === cat2)
    println(optionCat1 === optionCat2)
  }
}