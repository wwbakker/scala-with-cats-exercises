package nl.wwbakker.catssandbox.chapter2

object BooleanMonoids {
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  object Monoid {
    def apply[A](implicit monoid: Monoid[A]) : Monoid[A] =
      monoid
  }


  val BooleanAndMonoid : Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }
  val BooleanOrMonoid : Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  val BooleanXorMonoid : Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
  }
}


object SuperAdder {
  import cats.Monoid
  import cats.syntax.semigroup._
  import cats.instances.int._
  import cats.instances.option._
  import cats.instances.double._

  case class Order(totalCost : Double, quantity: Double)
  object Order {
    implicit val OrderInstance : Monoid[Order] = new Monoid[Order]{
      override def empty: Order = Order(0d, 0d)
      override def combine(x: Order, y: Order): Order =
        Order(x.totalCost |+| y.totalCost, x.quantity |+| y.quantity)
    }
  }


  def add[A : Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)

//  def add(items : List[Int]) : Int = add(items)

//  def addOptionInt(items : List[Option[Int]]) : Option[Int] = add(items)

}
