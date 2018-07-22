package com.github.plippe.suprnation

import cats.{MonadError, Order}
import cats.implicits._
import cats.data.NonEmptyList

trait SolverError extends Throwable

trait Solver[T, F[_]] {
  def solve(problem: NonEmptyList[String]): F[NonEmptyList[Node[T]]]
}

trait TreeSolver[T, V, F[_]] extends Solver[T, F] {
  implicit val F: MonadError[F, Throwable]
  implicit val O: Order[V]

  val parser: Parser[T, F]
  val combiner: Combiner[T, F]

  def best(elements: NonEmptyList[Node[T]]): V

  def solve(problem: NonEmptyList[String]): F[NonEmptyList[Node[T]]] = {
    def recursive(solution: List[NonEmptyList[Node[T]]],
                  head: String,
                  tail: List[String]): F[List[NonEmptyList[Node[T]]]] = {
      for {
        elements <- parser.parse(head)
        solutionsPlusOne <- combiner.prepend(solution, elements)
        bestSolutionsPlusOne = solutionsPlusOne
          .groupBy(_.head.index)
          .values
          .map(_.sortBy(best).head)
          .toList
        bestSolutionsPlusN <- tail match {
          case head :: tail => recursive(bestSolutionsPlusOne, head, tail)
          case Nil          => bestSolutionsPlusOne.pure[F]
        }
      } yield bestSolutionsPlusN
    }

    recursive(List.empty[NonEmptyList[Node[T]]], problem.head, problem.tail)
      .map(_.sortBy(best).head.reverse)
  }
}

class ShortestTreeSolver[T, F[_]](val parser: Parser[T, F],
                                  val combiner: Combiner[T, F])(
    implicit val F: MonadError[F, Throwable],
    implicit val O: Order[T],
    implicit val NT: Numeric[T]
) extends TreeSolver[T, T, F] {

  override def best(elements: NonEmptyList[Node[T]]): T = {
    elements.map(_.value).toList.sum
  }

}
