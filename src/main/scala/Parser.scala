package com.github.plippe.suprnation

import cats.implicits._
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Valid, Invalid}

trait ParserValidation extends Throwable
case object EmptyLine extends ParserValidation
case class ParserCastError(e: Throwable) extends ParserValidation

trait Parser[T] {
  type Result[A] = Validated[ParserValidation, A]

  def cast(str: String): Result[T]

  def parse(line: String): Result[NonEmptyList[Node[T]]] = {
    val empty = List.empty[T].valid[ParserValidation]
    line
      .split(" ")
      .map(cast)
      .foldLeft(empty) {
        case (err @ Invalid(_), _)   => err
        case (_, Invalid(err))       => err.invalid[List[T]]
        case (Valid(agg), Valid(el)) => (agg :+ el).valid
      }
      .andThen { list =>
        Validated.fromOption(NonEmptyList.fromList(list), EmptyLine)
      }
      .map(_.map(value => Node(value)))
  }
}

class IntParser extends Parser[Int] {
  override def cast(str: String): Result[Int] = {
    try {
      str.toInt.valid[ParserCastError]
    } catch {
      case e: Throwable => ParserCastError(e).invalid[Int]
    }
  }
}
