package nl.wwbakker.catssandbox.chapter5


import cats.data.EitherT
import cats.instances.future._
import concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}

object A {
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  def getPowerLevel(name : String) : Response[Int] =
    powerLevels.get(name) match {
      case Some(value) => EitherT.right(Future.successful(value))
      case None => EitherT.left(Future.successful(s"Powerlevel of $name could not be found."))
    }


  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      powerLevel1 <- getPowerLevel(ally1)
      powerLevel2 <- getPowerLevel(ally2)
    } yield (powerLevel1 + powerLevel2) > 15


  def tacticalReport(ally1: String, ally2: String) : Unit = {
    Await.result(canSpecialMove(ally1, ally2).value, 1.second) match {
      case Left(_) | Right(false)  => println(s"$ally1 and $ally2 cannot do their special move.")
      case Right(true) => println(s"$ally1 and $ally2 can do their special move.")
    }
  }
}
