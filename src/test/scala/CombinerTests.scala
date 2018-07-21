package com.github.plippe.suprnation

import cats.data.NonEmptyList
import utest._

object CombinerTests extends TestSuite {

  val tests = Tests {
    "treeCombiner" - {
      "combine two lists" - { combine() }
      "fails combining lists of wrong sizes" - { combineWrongLists() }
    }
  }

  def combine() = {
    val combiner = new TreeCombiner[Int]
    val lists = NonEmptyList.of(NonEmptyList.of(2, 1), NonEmptyList.of(8, 9))
    val nodes = NonEmptyList.of(3, 5, 7)

    val result = combiner.prepend(lists, nodes)
    assert(result.isValid)
    assert(
      result.exists(
        _ == NonEmptyList.of(
          NonEmptyList.of(3, 2, 1),
          NonEmptyList.of(5, 2, 1),
          NonEmptyList.of(5, 8, 9),
          NonEmptyList.of(7, 8, 9),
        )))
  }

  def combineWrongLists() = {
    val combiner = new TreeCombiner[Int]
    val lists = NonEmptyList.of(NonEmptyList.of(1))
    val nodes = NonEmptyList.of(2, 3, 4)

    val result = combiner.prepend(lists, nodes)
    assert(result.isInvalid)
    assert(result.swap.exists(_.isInstanceOf[CombinerValidation]))
  }

}
