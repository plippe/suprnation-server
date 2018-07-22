package com.github.plippe.suprnation.io

import cats.implicits._
import cats.Applicative
import cats.effect.Sync
import java.util.concurrent.atomic.AtomicInteger
import scala.util.Random

trait Reader[F[_]] {
  def readLine(): F[Option[String]]
}

class StandardInputReader[F[_]]()(implicit F: Sync[F]) extends Reader[F] {
  def readLine(): F[Option[String]] = F.delay {
    scala.io.StdIn.readLine() match {
      case null =>
        None // The string read from the terminal or null if the end of stream was reached.
      case ""   => None
      case line => line.some
    }
  }
}

class IteratorReader[F[_]](lines: Iterator[String])(implicit F: Applicative[F])
    extends Reader[F] {

  def readLine(): F[Option[String]] = {
    lines.hasNext match {
      case false => none[String].pure[F]
      case true  => lines.next.some.pure[F]
    }
  }

}

class RandomTreeReader[F[_]](seed: Int, maxLine: Int)(
    implicit F: Applicative[F])
    extends Reader[F] {

  val maxValue = 10

  val rnd = new Random(seed)
  val currentLine = new AtomicInteger(1)

  def readLine(): F[Option[String]] = {
    currentLine.get() match {
      case line if line > maxLine => none[String].pure[F]
      case line =>
        currentLine.compareAndSet(line, line + 1)
        1.to(line).map(_ => rnd.nextInt(maxValue)).mkString(" ").some.pure[F]
    }
  }

}
