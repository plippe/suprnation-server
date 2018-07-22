package com.github.plippe.suprnation

import cats.{MonadError, Order}
import cats.implicits._
import cats.data.NonEmptyList

import com.github.plippe.suprnation.io.Reader

trait SolverError extends Throwable
case object SolverNoInput extends SolverError

trait Solver[T, F[_]] {
  def solve(): F[NonEmptyList[Node[T]]]
}

trait TreeSolver[T, V, F[_]] extends Solver[T, F] {
  implicit val F: MonadError[F, Throwable]
  implicit val O: Order[V]

  val reader: Reader[F]
  val parser: Parser[T, F]
  val combiner: Combiner[T, F]

  def best(elements: NonEmptyList[Node[T]]): V

  def solve(): F[NonEmptyList[Node[T]]] = {
    def recursive(solution: List[NonEmptyList[Node[T]]],
                  line: String): F[List[NonEmptyList[Node[T]]]] = {
      for {
        elements <- parser.parse(line)
        solutionsPlusOne <- combiner.prepend(solution, elements)
        bestSolutionsPlusOne = solutionsPlusOne
          .groupBy(_.head.index)
          .values
          .map(_.sortBy(best).head)
          .toList
        newLine <- reader.readLine()
        bestSolutionsPlusN <- newLine match {
          case Some(newLine) => recursive(bestSolutionsPlusOne, newLine)
          case None          => bestSolutionsPlusOne.pure[F]
        }
      } yield bestSolutionsPlusN
    }

    for {
      line <- reader.readLine()
      solutions <- line match {
        case Some(line) => recursive(List.empty[NonEmptyList[Node[T]]], line)
        case None       => F.raiseError(SolverNoInput)
      }
    } yield {
      solutions.sortBy(best).head.reverse
    }
  }
}

class ShortestTreeSolver[T, F[_]](val reader: Reader[F],
                                  val parser: Parser[T, F],
                                  val combiner: Combiner[T, F])(
    implicit val F: MonadError[F, Throwable],
    implicit val O: Order[T],
    implicit val NT: Numeric[T]
) extends TreeSolver[T, T, F] {

  override def best(elements: NonEmptyList[Node[T]]): T = {
    elements.map(_.value).toList.sum
  }

}
