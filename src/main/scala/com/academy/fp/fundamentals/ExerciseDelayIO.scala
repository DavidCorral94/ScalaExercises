package com.fp.fundamentals

import cats.effect.{IO, Sync}

import java.io.OutputStream

object ExerciseDelayIO {

  def printlnF[F[_]](outputStream: OutputStream)(s: String)(implicit F: Sync[F]): F[Unit] = F.delay(
    Console.withOut(outputStream) {
      println(s)
    }
  )

  def main(args: Array[String]): Unit = {
    printlnF[IO](null)("Hello!")
  }

}
