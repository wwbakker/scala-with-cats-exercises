package nl.wwbakker.catssandbox.chapter1

trait Printable[A] {
  def format(a : A) : String
}

object PrintableInstances {
  implicit val stringPrintable : Printable[String] = new Printable[String] {
    override def format(a: String): String = a
  }
  implicit val intPrintable : Printable[Int] = new Printable[Int] {
    override def format(a: Int): String = a.toString
  }

  implicit val catPrintable : Printable[Cat] = new Printable[Cat] {
    override def format(a: Cat): String = s"${Printable.format(a.name)} is a ${Printable.format(a.age)} year-old ${Printable.format(a.color)} cat."
  }
}

object Printable {
  def format[A](a : A)(implicit printable: Printable[A]) : String = printable.format(a)
  def print[A](a : A)(implicit printable: Printable[A]) : Unit = println(format(a))
}

object PrintableSyntax {
  implicit class PrintableOps[A](a : A) {
    def format(implicit printable : Printable[A]) : String = printable.format(a)
    def print(implicit printable : Printable[A]) : Unit = println(printable.format(a))
  }
}


object Worksheet {
  import PrintableInstances._
  import PrintableSyntax._

  def run(): Unit = {
    val cat = Cat("Jan", 4, "blue")

    cat.print
  }
}