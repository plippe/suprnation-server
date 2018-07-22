package com.github.plippe.suprnation

import cats.implicits._
import cats.data.NonEmptyList
import utest._

import com.github.plippe.suprnation.io._

object MainTests extends TestSuite {

  val tests = Tests {
    "utest works" - { assert(0 == 0) }
    "solve example" - { example() }
  }

  type F[T] = Either[Throwable, T]

  def example() = {
    val reader =
      new IteratorReader[F](Iterator("7", "6 3", "3 8 5", "11 2 10 9"))
    val writer = new Writer[F] {
      def writeLine(str: String): F[Unit] = Right(())
    }

    val solver = new ShortestTreeSolver[Int, F](
      new IntParser[F](),
      new TreeCombiner[Int, F]()
    )

    val output = { solution: NonEmptyList[Node[Int]] =>
      assert(
        solution == NonEmptyList
          .of(Node(7, 0), Node(6, 0), Node(3, 0), Node(2, 1)))
      ""
    }

    val result = Main.build(reader, writer, solver, output)
    assert(result.isRight)
  }
}
