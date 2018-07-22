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
      case Nil => NonEmptyList.of(elements).pure[F]
      case _ =>
        val prependedLists = lists.flatMap { list =>
          val left = elements.find(_.index == list.head.index)
          val right = elements.find(_.index == list.head.index + 1)

          (left, right) match {
            case (Some(left), Some(right)) =>
              List(
                list.prepend(left),
                list.prepend(right),
              )

            case _ => List()
          }
        }

        if (prependedLists.size == lists.size * 2) {
          NonEmptyList.fromListUnsafe(prependedLists).pure[F]
        } else {
          F.raiseError(NonCombinable(lists, elements))
        }
    }

  }

}
