package nl.wwbakker.catssandbox.chapter4

import cats.data.State
import cats._
import cats.implicits._

object E {
  type CalcState[A] = State[List[Int], A]


  def operand(i : Int) : CalcState[Int] = State[List[Int],Int]{ stack =>
    (i :: stack, i)
  }

  def operator(fn : (Int, Int) => Int) : CalcState[Int] = State[List[Int], Int]{
    case a :: b :: tail =>
      val ans = fn(b, a)
      (ans :: tail, ans)
    case _ =>
      sys.error("you used it wrong!")
  }

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }


  def answer : CalcState[Int] = for {
    _ <- evalOne("5")
    _ <- evalOne("6")
    ans <- evalOne("*")
  } yield ans

  def evalAll(input: List[String]): CalcState[Int] =
    //input.tail.foldLeft(evalOne(input.head))((state, sym) => state.flatMap(_ => evalOne(sym)))
    input.foldLeft(0.pure[CalcState])((state, sym) => state.flatMap(_ => evalOne(sym)))

  def evalInput(input : String) : Int =
    evalAll(input.split(" ").toList).runA(Nil).value

//  def evalOne(sym: String): CalcState[Int] =
//    sym match {
//      case "+" => for {
//        operand1 :: operand2 :: Nil <- State.get[List[Int]].map(_.take(2))
//        _                           <- State.modify[List[Int]](_.drop(2))
//        _                           <- State.modify[List[Int]]((operand1 + operand2) +: _)
//        result                      <- State.inspect[List[Int], Int](_.head)
//      } yield result
//      case "*" =>
//      case _ => Integer.parseInt(sym)
//    }
//    State.pure[List[Int], Int](Integer.parseInt(sym))
}
