package com.github.plippe.suprnation

import cats.implicits._
import cats.data.NonEmptyList
import utest._

object CombinerTests extends TestSuite {

  type F[T] = Either[Throwable, T]

  val tests = Tests {
    "treeCombiner" - {
      "combine two lists" - { combine() }
      "combine empty list" - { combineEmpty() }
      "fails combining lists of wrong sizes" - { combineWrongLists() }
    }
  }

  def combine() = {
    val combiner = new TreeCombiner[Int, F]()
    val lists =
      List(NonEmptyList.of(Node(2, 0), Node(1, 0)),
           NonEmptyList.of(Node(8, 1), Node(9, 1)))
    val nodes = NonEmptyList.of(Node(3, 0), Node(5, 1), Node(7, 2))

    val result = combiner.prepend(lists, nodes)
    assert(result.isRight)
    assert(
      result.exists(_ == NonEmptyList.of(
        NonEmptyList.of(Node(3, 0), Node(2, 0), Node(1, 0)),
        NonEmptyList.of(Node(5, 1), Node(2, 0), Node(1, 0)),
        NonEmptyList.of(Node(5, 1), Node(8, 1), Node(9, 1)),
        NonEmptyList.of(Node(7, 2), Node(8, 1), Node(9, 1)),
      )))
  }

  def combineEmpty() = {
    val combiner = new TreeCombiner[Int, F]()
    val lists = List.empty
    val nodes = NonEmptyList.of(Node(1, 0))

    val result = combiner.prepend(lists, nodes)
    assert(result.isRight)
    assert(
      result.exists(
        _ == NonEmptyList.of(
          NonEmptyList.of(Node(1, 0))
        )))
  }

  def combineWrongLists() = {
    val combiner = new TreeCombiner[Int, F]()
    val lists = List.empty
    val nodes = NonEmptyList.of(Node(1, 0), Node(2, 0))

    val result = combiner.prepend(lists, nodes)
    assert(result.isLeft)
    assert(result.swap.exists(_.isInstanceOf[CombinerError]))
  }

}
