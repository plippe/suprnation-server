package com.github.plippe.suprnation.io

import cats.implicits._
import cats.Applicative
import cats.effect.Sync

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
