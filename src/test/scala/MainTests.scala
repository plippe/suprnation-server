package com.github.plippe.suprnation

import cats.implicits._
import cats.data.NonEmptyList
import utest._

import com.github.plippe.suprnation.io._

object MainTests extends TestSuite {

  val tests = Tests {
    "utest works" - { assert(0 == 0) }
    "solve example" - { example() }
    "random 2" - { random(0, 2) }
    "random 11" - { random(0, 11) }
    "random 23" - { random(0, 23) }
    "random 31" - { random(0, 31) }
    "random 41" - { random(0, 41) }
    "random 53" - { random(0, 53) }
    "random 61" - { random(0, 61) }
    "random 71" - { random(0, 71) }
    "random 83" - { random(0, 83) }
    "random 97" - { random(0, 97) }
    "random 101" - { random(0, 101) }
    "random 211" - { random(0, 211) }
    "random 307" - { random(0, 307) }
    "random 401" - { random(0, 401) }
    "random 503" - { random(0, 503) }
  }

  type F[T] = Either[Throwable, T]

  def example() = {
    val solver = new ShortestTreeSolver[Int, F](
      new IteratorReader[F](Iterator("7", "6 3", "3 8 5", "11 2 10 9")),
      new IntParser[F](),
      new TreeCombiner[Int, F]()
    )

    val writer = new Writer[F] {
      def writeLine(str: String): F[Unit] = Right(())
    }
    val output = { solution: NonEmptyList[Node[Int]] =>
      assert(
        solution == NonEmptyList
          .of(Node(7, 0), Node(6, 0), Node(3, 0), Node(2, 1)))
      ""
    }

    val result = Main.build(solver, writer, output)
    assert(result.isRight)
  }

  def random(seed: Int, maxLine: Int) = {
    val solver = new ShortestTreeSolver[Int, F](
      new RandomTreeReader[F](seed, maxLine),
      new IntParser[F](),
      new TreeCombiner[Int, F]()
    )

    val writer = new Writer[F] {
      def writeLine(str: String): F[Unit] = Right(())
    }
    val output = { solution: NonEmptyList[Node[Int]] =>
      ""
    }

    val result = Main.build(solver, writer, output)
    assert(result.isRight)
  }
}
