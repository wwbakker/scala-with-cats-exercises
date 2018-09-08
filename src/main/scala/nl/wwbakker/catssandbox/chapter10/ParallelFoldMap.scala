package nl.wwbakker.catssandbox.chapter10

import cats.Monoid
import nl.wwbakker.catssandbox.chapter9.FoldMap

import scala.concurrent.Future
import cats.instances.list._
import cats.instances.future._
import cats.syntax.traverse._

object ParallelFoldMap {
  implicit val ec = scala.concurrent.ExecutionContext.global
  def parallelFoldMap[A, B : Monoid]
    (values: Vector[A])
    (func: A => B): Future[B] = {
    val numberOfProcessors = Runtime.getRuntime.availableProcessors()
    val groupedValues = values.grouped(values.size / numberOfProcessors)
    val groupedResults : List[Future[B]] =
      groupedValues.map(valuesPerProcessor => Future(FoldMap.foldMap(valuesPerProcessor)(func))).toList
    groupedResults.sequence

  }

  def exampleF(a : Int) : String = {
    Thread.sleep(100)
    a.toString
  }
}
