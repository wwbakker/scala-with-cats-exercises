package nl.wwbakker.catssandbox.chapter4
import cats._



object F {
  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A])
    extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)


  implicit val treeMonad : Monad[Tree] = new Monad[Tree] {

    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Leaf(x) => f(x)
        case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      }

    override def tailRecM[A, B](a: A)(fn: A => Tree[Either[A, B]]): Tree[B] = {
      def eitherHelper(either : Either[A, B]) : Tree[B] = {
        either match {
          case Left(a1) => tailRecM(a1)(fn)
          case Right(b) => pure(b)
        }
      }

      fn(a) match {
        case Leaf(x) => eitherHelper(x)
        case Branch(x, y) => Branch(flatMap(x)(eitherHelper), flatMap(y)(eitherHelper))
      }
    }
  }
}