package com.github.plippe.suprnation

import cats.implicits._
import cats.data.{NonEmptyList, Validated}

trait CombinerValidation extends Throwable
case class NonCombinable[T](lists: NonEmptyList[NonEmptyList[T]],
                            elements: NonEmptyList[T])
    extends CombinerValidation

trait Combiner[T] {
  type Result[A] = Validated[CombinerValidation, A]

  def prepend(lists: NonEmptyList[NonEmptyList[T]],
              elements: NonEmptyList[T]): Result[NonEmptyList[NonEmptyList[T]]]
}

class TreeCombiner[T] extends Combiner[T] {
  override def prepend(
      lists: NonEmptyList[NonEmptyList[T]],
      elements: NonEmptyList[T]): Result[NonEmptyList[NonEmptyList[T]]] = {
    if (lists.size + 1 != elements.size) NonCombinable(lists, elements).invalid
    else {
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
              left :: list,
              right :: list
            )
        }

      NonEmptyList.fromListUnsafe(prependedLists).valid
    }
  }
}
