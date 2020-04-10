package com.demo.free

import cats.arrow.FunctionK
import cats.Monad
import Free._

sealed trait Free[F[_], A] extends Product with Serializable {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
  def map[B](f: A => B): Free[F, B] = this.flatMap(a => pure(f(a)))

  final def foldMap[M[_]](nt: FunctionK[F, M])(implicit M: Monad[M]): M[A] = this match {
    case Pure(a)            => M.pure(a)
    case Suspend(fa)        => nt(fa)
    case FlatMap(self, foo) => M.flatMap(self.foldMap(nt))(cc => foo(cc).foldMap(nt)) //compile (phantom type)
  }
}

object Free {
  def pure[F[_], A](a: A): Free[F, A] = Pure(a)
  def suspend[F[_], A](fa: F[A]): Free[F, A] = Suspend(fa)

  final case class Pure[F[_], A](value: A) extends Free[F, A]
  final case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
  final case class FlatMap[F[_], A, B](self: Free[F, A], f: A => Free[F, B]) extends Free[F, B]
}






