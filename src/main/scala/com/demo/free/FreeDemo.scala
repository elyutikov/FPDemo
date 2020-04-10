package com.demo.free

import com.demo.free.Disk.{Delete, Read, Write}
import java.nio.file.{Files, Path, Paths}
import cats.arrow.FunctionK
import cats.effect.IO

object FreeDemo extends App {
  def program: Free[Disk, Unit] = for {
    contents <- Free.suspend(Read("hello.txt"))
    path     <- Free.suspend(Write("hello.txt", contents ++ "test".getBytes))
    _        <- Free.suspend(Write("logs.txt", "did a thing".getBytes))
    _        <- Free.suspend(Delete("evil.txt"))
  } yield ()

  val nt: FunctionK[Disk, IO] = new FunctionK[Disk, IO] {
    def apply[A](da: Disk[A]): IO[A] = da match {
      case Read(file)         => IO { Files.readAllBytes(Paths.get(file)) }
      case Write(file, bytes) => IO { Files.write(Paths.get(file), bytes) }
      case Delete(file)       => IO { Files.delete(Paths.get(file)) }
    }
  }
}

sealed trait Disk[A]
object Disk {
  final case class Read(file: String) extends Disk[Array[Byte]]
  final case class Write(file: String, bytes: Array[Byte]) extends Disk[Path]
  final case class Delete(file: String) extends Disk[Unit]
}

trait DiskT[A] {
  def read(file: String): Array[Byte]
  def write(file: String): Path
  def delete(file: String): Unit
}

