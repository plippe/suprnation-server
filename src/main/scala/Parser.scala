package com.github.plippe.suprnation

import cats.MonadError
import cats.implicits._
import cats.data.{NonEmptyList}

trait ParserValidation extends Throwable
case object EmptyLine extends ParserValidation
case class ParserCastError(e: Throwable) extends ParserValidation

trait Parser[T, F[_]] {
  implicit val F: MonadError[F, ParserValidation]

  def cast(str: String): F[T]

  def parse(line: String): F[NonEmptyList[T]] = {
    line
      .split(" ")
      .toList
      .traverse(cast)
      .flatMap { list =>
        NonEmptyList
          .fromList(list)
          .fold(F.raiseError[NonEmptyList[T]](EmptyLine))(_.pure[F])
      }
  }

}

class IntParser[F[_]]()(implicit val F: MonadError[F, ParserValidation])
    extends Parser[Int, F] {

  override def cast(str: String): F[Int] = {
    try {
      str.toInt.pure[F]
    } catch {
      case e: Throwable => F.raiseError(ParserCastError(e))
    }
  }

}
