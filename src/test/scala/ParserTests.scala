package com.github.plippe.suprnation

import cats.implicits._
import cats.data.NonEmptyList
import utest._

object ParserTests extends TestSuite {

  type F[T] = Either[Throwable, T]

  val tests = Tests {
    "parser" - {
      "parse casted" - { parseCasted() }
      "fails parsing after failed cast" - { parseFailedCast() }
    }
    "parserInt" - {
      "casts int" - { castInt() }
      "fail cast" - { castFail() }
    }
  }

  def parseCasted() = {
    val parser = new Parser[Int, F] {
      implicit val F =
        cats.instances.either.catsStdInstancesForEither[Throwable]
      def cast(str: String) = Right(0)
    }

    val result = parser.parse("1 2 3")
    assert(result.isRight)
    assert(result.exists(_ == NonEmptyList.of(0, 0, 0)))
  }

  def parseFailedCast() = {
    case object MyParserValidation extends Throwable
    val parser = new Parser[Int, F] {
      implicit val F =
        cats.instances.either.catsStdInstancesForEither[Throwable]
      def cast(str: String) = Left(MyParserValidation)
    }

    val result = parser.parse("1 2 3")
    assert(result.isLeft)
    assert(result.swap.exists(_ == MyParserValidation))
  }

  def castInt() = {
    val parser = new IntParser[F]()
    val result = parser.parse("1 2 3")
    assert(result.isRight)
    assert(result.exists(_ == NonEmptyList.of(1, 2, 3)))
  }

  def castFail() = {
    val parser = new IntParser[F]()
    val result = parser.parse("1 2 x")
    assert(result.isLeft)
    assert(result.swap.exists(_.isInstanceOf[ParserError]))
  }

}
