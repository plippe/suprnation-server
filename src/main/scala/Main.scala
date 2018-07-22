package com.github.plippe.suprnation

import cats.MonadError
import cats.implicits._
import cats.data.NonEmptyList
import cats.effect.IO

import com.github.plippe.suprnation.io._

object Main {

  def main(args: Array[String]): Unit = {
    val solver = {
      val reader = new StandardInputReader[IO]()
      val parser = new IntParser[IO]()
      val combiner = new TreeCombiner[Int, IO]()
      new ShortestTreeSolver[Int, IO](reader, parser, combiner)
    }

    val writer = new StandardOutputWriter[IO]()
    val output = { solution: NonEmptyList[Node[Int]] =>
      val ints = solution.map(_.value).toList
      s"Minimal path is: ${ints.mkString(" + ")} = ${ints.sum}"
    }

    build(solver, writer, output).unsafeRunSync
  }

  def build[T, F[_]](solver: Solver[T, F],
                     writer: Writer[F],
                     output: NonEmptyList[Node[T]] => String)(
      implicit F: MonadError[F, Throwable]): F[Unit] = {

    for {
      solution <- solver.solve()
      solutionStr = output(solution)
      _ <- writer.writeLine(solutionStr)
    } yield ()
  }

}
