package com.github.plippe.suprnation.io

import cats.effect.Sync

trait Reader[F[_]] {
  def readLine(): F[Option[String]]
}

class StandardInputReader[F[_]]()(implicit F: Sync[F]) extends Reader[F] {
  def readLine(): F[Option[String]] = F.delay {
    val line = scala.io.StdIn.readLine()
    Option(line) // The string read from the terminal or null if the end of stream was reached.
  }
}
