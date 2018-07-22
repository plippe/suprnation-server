package com.github.plippe.suprnation

import cats.implicits._
import cats.instances._
import cats.data.NonEmptyList
import utest._

object SolverTests extends TestSuite {

  type F[T] = Either[Throwable, T]
  val FImpl = either.catsStdInstancesForEither[Throwable]
  val OImpl = int.catsKernelStdOrderForInt

  val tests = Tests {
    "TreeSolver" - {
      "find first best" - { findFirstBest() }
      "fail parser error" - { failParserError() }
      "fail combiner error" - { failCombinerError() }
    }
    "ShortestTreeSolver" - {
      "best" - { shortestSolverBest() }
    }
  }

  def findFirstBest() = {
    var parserCalls = 0
    var combinerCalls = 0

    val solver = new TreeSolver[Int, Int, F] {
      implicit val F = FImpl
      implicit val O = OImpl

      val parser = new Parser[Int, F] {
        implicit val F = FImpl
        def cast(str: String) = {
          parserCalls += 1
          Right(0)
        }
      }
      val combiner = new Combiner[Int, F] {
        def prepend(lists: List[NonEmptyList[Node[Int]]],
                    elements: NonEmptyList[Node[Int]]) = {
          combinerCalls += 1
          Right(NonEmptyList.of(NonEmptyList.of(Node(0, 0))))
        }
      }

      def best(elements: NonEmptyList[Node[Int]]): Int = 0
    }

    val calls = 5
    val result = solver.solve(NonEmptyList.fromListUnsafe(List.fill(5)("")))

    assert(result.isRight)
    assert(parserCalls == calls)
    assert(combinerCalls == calls)
  }

  def failParserError() = {
    val solver = new TreeSolver[Int, Int, F] {
      implicit val F = FImpl
      implicit val O = OImpl

      val parser = new Parser[Int, F] {
        implicit val F = FImpl
        def cast(str: String) = Left(new Throwable)
      }
      val combiner = new Combiner[Int, F] {
        def prepend(lists: List[NonEmptyList[Node[Int]]],
                    elements: NonEmptyList[Node[Int]]) = {
          Right(NonEmptyList.of(NonEmptyList.of(Node(0, 0))))
        }
      }

      def best(elements: NonEmptyList[Node[Int]]): Int = 0
    }

    val result = solver.solve(NonEmptyList.fromListUnsafe(List.fill(5)("")))
    assert(result.isLeft)
  }

  def failCombinerError() = {
    val solver = new TreeSolver[Int, Int, F] {
      implicit val F = FImpl
      implicit val O = OImpl

      val parser = new Parser[Int, F] {
        implicit val F = FImpl
        def cast(str: String) = Right(0)
      }
      val combiner = new Combiner[Int, F] {
        def prepend(lists: List[NonEmptyList[Node[Int]]],
                    elements: NonEmptyList[Node[Int]]) = {
          Left(new Throwable)
        }
      }

      def best(elements: NonEmptyList[Node[Int]]): Int = 0
    }

    val result = solver.solve(NonEmptyList.fromListUnsafe(List.fill(5)("")))
    assert(result.isLeft)
  }

  def shortestSolverBest() = {
    val solver = new ShortestTreeSolver[Int, F](
      parser = new Parser[Int, F] {
        implicit val F = FImpl
        def cast(str: String) = Right(0)
      },
      combiner = new Combiner[Int, F] {
        def prepend(lists: List[NonEmptyList[Node[Int]]],
                    elements: NonEmptyList[Node[Int]]) = {
          Right(NonEmptyList.of(NonEmptyList.of(Node(0, 0))))
        }
      }
    )

    val result =
      solver.best(NonEmptyList.fromListUnsafe(List.fill(5)(Node(1, 0))))
    assert(result == 5)
  }
}
