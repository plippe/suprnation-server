package com.github.plippe.suprnation

import cats.ApplicativeError
import cats.data.NonEmptyList
import cats.implicits._

trait CombinerValidation extends Throwable
case class NonCombinable[T](lists: NonEmptyList[NonEmptyList[T]],
                            elements: NonEmptyList[T])
    extends CombinerValidation

trait Combiner[T, F[_]] {
  def prepend(lists: NonEmptyList[NonEmptyList[T]],
              elements: NonEmptyList[T]): F[NonEmptyList[NonEmptyList[T]]]
}

class TreeCombiner[T, F[_]]()(
    implicit val F: ApplicativeError[F, CombinerValidation])
    extends Combiner[T, F] {

  override def prepend(
      lists: NonEmptyList[NonEmptyList[T]],
      elements: NonEmptyList[T]): F[NonEmptyList[NonEmptyList[T]]] = {

    if (lists.size + 1 != elements.size) {
      F.raiseError(NonCombinable(lists, elements))
    } else {
      val leftRightElements = elements.toList
        .sliding(2)
        .toList
        .collect {
          case left :: right :: Nil => (left, right)
        }

      val prependedLists = lists.toList
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
