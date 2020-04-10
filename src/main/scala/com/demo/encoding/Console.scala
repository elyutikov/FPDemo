package com.demo.encoding

trait ConsoleAlg[F[_]] {
  def readLine: F[Option[String]]
  def printLine(line: String): F[Unit]
}


trait Console[+A]
case object ReadLine extends Console[Option[String]]
case class PrintLine(line: String) extends Console[Unit]


trait ConsoleT[+A] {
  def run[F[+_]](F: ConsoleAlg[F]): F[A]
}

object ConsoleT {
  val readLine: ConsoleT[Option[String]] = new ConsoleT[Option[String]] {
    def run[F[+_]](F: ConsoleAlg[F]): F[Option[String]] = F.readLine
  }

  def printLine(line: String): ConsoleT[Unit] = new ConsoleT[Unit] {
    def run[F[+_]](F: ConsoleAlg[F]): F[Unit] = F.printLine(line)
  }
}

// more generally
trait Term[Alg[_[_]], +A] {
  def apply[F[+_]](A: Alg[F]): F[A]
}

object Term {
  def readLine: Term[ConsoleAlg, Option[String]] = new Term[ConsoleAlg, Option[String]] {
    def apply[F[_]](A: ConsoleAlg[F]): F[Option[String]] = A.readLine
  }
  // etc
}