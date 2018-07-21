package com.github.plippe.suprnation.io

import cats.effect.Sync

trait Writer[F[_]] {
  def writeLine(str: String): F[Unit]
}

class StandardOutputWriter[F[_]]()(implicit F: Sync[F]) extends Writer[F] {
  def writeLine(str: String): F[Unit] = F.delay {
    println(str)
  }
}
