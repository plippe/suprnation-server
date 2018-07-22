package com.github.plippe.suprnation

import cats.{MonadError, Order}
import cats.implicits._
import cats.data.NonEmptyList

trait SolverError extends Throwable

trait Solver[T, V, F[_]] {
  implicit val F: MonadError[F, Throwable]
  implicit val OT: Order[T]
  implicit val OV: Order[V]

  val parser: Parser[T, F]
  val combiner: Combiner[T, F]

  def best(elements: NonEmptyList[T]): V

  def solve(problem: NonEmptyList[String]): F[NonEmptyList[T]] = {
    def recursive(solution: List[NonEmptyList[T]],
                  head: String,
                  tail: List[String]): F[List[NonEmptyList[T]]] = {
      for {
        elements <- parser.parse(head)
        solutionsPlusOne <- combiner.prepend(solution, elements)
        bestSolutionsPlusOne = solutionsPlusOne
          .groupBy(_.head)
          .values
          .map(_.sortBy(best).head)
          .toList
        bestSolutionsPlusN <- tail match {
          case head :: tail => recursive(bestSolutionsPlusOne, head, tail)
          case Nil          => bestSolutionsPlusOne.pure[F]
        }
      } yield bestSolutionsPlusN
    }

    recursive(List.empty[NonEmptyList[T]], problem.head, problem.tail)
      .map(_.sortBy(best).head.reverse)
  }
}

class ShortestPathSolver[T, F[_]](val parser: Parser[T, F],
                                  val combiner: Combiner[T, F])(
    implicit val F: MonadError[F, Throwable],
    implicit val OT: Order[T],
    implicit val OV: Order[T],
    implicit val NT: Numeric[T]
) extends Solver[T, T, F] {

  override def best(elements: NonEmptyList[T]): T = {
    elements.toList.sum
  }

}
