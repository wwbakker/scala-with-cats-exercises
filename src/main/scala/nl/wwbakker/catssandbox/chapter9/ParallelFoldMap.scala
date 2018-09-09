package nl.wwbakker.catssandbox.chapter9

import cats.Monoid

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats.instances.future._

object ParallelFoldMap {
  def parallelFoldMap[A, B : Monoid]
  (values: Vector[A])
  (func: A => B): Future[B] = {
    val numberOfProcessors = Runtime.getRuntime.availableProcessors()
    val groupedValues = values.grouped(Math.ceil(values.size / numberOfProcessors).toInt)
    FoldMap.foldMap(groupedValues.toVector)(valuesPerProcessor => Future(FoldMap.foldMap(valuesPerProcessor)(func)))
  }


  def exampleF(a : Int) : String = {
    Thread.sleep(100)
    a.toString
  }
}
