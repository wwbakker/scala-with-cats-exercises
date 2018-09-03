package nl.wwbakker.catssandbox.chapter4

import cats._
import cats.data._
import cats.implicits._

object D {
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil =>
        acc
    }


  def stackSafeFoldRight[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, stackSafeFoldRight(tail, acc)(fn)))
      case Nil =>
        acc
    }


  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  type Logged[A] = Writer[Vector[String], A]

  def factorialWriter(in: Int): Logged[Int] =
    for {
      ans <- slowly{
        if (in == 0) 1.pure[Logged]
        else factorialWriter(in - 1).map(in * _)
      }
      _   <- Vector(s"fact $in $ans").tell
    } yield ans

  case class Db(
                 usernames: Map[Int, String],
                 passwords: Map[String, String]
               )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(_.usernames.get(userId))

  def checkPassword(
                     username: String,
                     password: String): DbReader[Boolean] =
    Reader(_.passwords.get(username).contains(password))

  def checkLogin(
                  userId: Int,
                  password: String): DbReader[Boolean] =
    for {
      usernameOption  <- findUsername(userId)
      passwordCorrect <- usernameOption.map(username => checkPassword(username, password)).getOrElse(false.pure[DbReader])
    } yield passwordCorrect


  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade"  -> "zerocool",
    "kate"  -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)
}