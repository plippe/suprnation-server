package com.github.plippe.suprnation

import cats.implicits._
import cats.data.NonEmptyList
import utest._

object ParserTests extends TestSuite {

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
    val parser = new Parser[Int] {
      def cast(str: String) = 0.valid[ParserValidation]
    }

    val result = parser.parse("1 2 3")
    assert(result.isValid)
    assert(result.exists(_ == NonEmptyList.of(Node(0), Node(0), Node(0))))
  }

  def parseFailedCast() = {
    case object MyParserValidation extends ParserValidation
    val parser = new Parser[Int] {
      def cast(str: String) = MyParserValidation.invalid
    }

    val result = parser.parse("1 2 3")
    assert(result.isInvalid)
    assert(result.swap.exists(_ == MyParserValidation))
  }

  def castInt() = {
    val parser = new IntParser()
    val result = parser.parse("1 2 3")
    assert(result.isValid)
    assert(result.exists(_ == NonEmptyList.of(Node(1), Node(2), Node(3))))
  }

  def castFail() = {
    val parser = new IntParser()
    val result = parser.parse("1 2 x")
    assert(result.isInvalid)
    assert(result.swap.exists(_.isInstanceOf[ParserCastError]))
  }

}
