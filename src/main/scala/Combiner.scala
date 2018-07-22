package com.github.plippe.suprnation

import cats.ApplicativeError
import cats.data.NonEmptyList
import cats.implicits._

trait CombinerError extends Throwable
case class NonCombinable[T](lists: List[NonEmptyList[Node[T]]],
                            elements: NonEmptyList[Node[T]])
    extends CombinerError

trait Combiner[T, F[_]] {
  def prepend(
      lists: List[NonEmptyList[Node[T]]],
      elements: NonEmptyList[Node[T]]): F[NonEmptyList[NonEmptyList[Node[T]]]]
}

class TreeCombiner[T, F[_]]()(implicit val F: ApplicativeError[F, Throwable])
    extends Combiner[T, F] {

  override def prepend(lists: List[NonEmptyList[Node[T]]],
                       elements: NonEmptyList[Node[T]])
    : F[NonEmptyList[NonEmptyList[Node[T]]]] = {

    lists match {
      case _ if lists.size + 1 != elements.size =>
        F.raiseError(NonCombinable(lists, elements))
      case Nil => NonEmptyList.of(elements).pure[F]
      case _ =>
        val leftRightElements = elements.toList
          .sliding(2)
          .toList
          .collect {
            case left :: right :: Nil => (left, right)
          }

        val prependedLists = lists
          .zip(leftRightElements)
          .flatMap {
            case (list, (left, right)) =>
              List(
                list.prepend(left),
                list.prepend(right),
              )
          }

        NonEmptyList.fromListUnsafe(prependedLists).pure[F]
    }

  }

}
