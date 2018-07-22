package com.github.plippe.suprnation

import cats.MonadError
import cats.implicits._
import cats.data.{NonEmptyList}

trait ParserError extends Throwable
case object ParserEmptyLine extends ParserError
case class ParserCastError(e: Throwable) extends ParserError

trait Parser[T, F[_]] {
  implicit val F: MonadError[F, Throwable]

  def cast(str: String): F[T]

  def parse(line: String): F[NonEmptyList[Node[T]]] = {
    line
      .split(" ")
      .toList
      .traverse(cast)
      .flatMap { list =>
        NonEmptyList
          .fromList(list)
          .fold(F.raiseError[NonEmptyList[T]](ParserEmptyLine))(_.pure[F])
          .map {
            _.zipWithIndex
              .map { case (value, index) => Node(value, index) }
          }
      }
  }

}

class IntParser[F[_]]()(implicit val F: MonadError[F, Throwable])
    extends Parser[Int, F] {

  override def cast(str: String): F[Int] = {
    try {
      str.toInt.pure[F]
    } catch {
      case e: Throwable => F.raiseError(ParserCastError(e))
    }
  }

}
