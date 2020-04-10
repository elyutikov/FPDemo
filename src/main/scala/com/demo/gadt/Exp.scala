package com.demo.gadt

import cats.Functor

sealed trait Exp[A] extends Product with Serializable

object Exp {
  final case class IntValue[A](v: Int) extends Exp[A]
  final case class DecValue[A](v: Double) extends Exp[A]
  final case class Sum[A](exp1: A, exp2: A) extends Exp[A]
  final case class Multiply[A](exp1: A, exp2: A) extends Exp[A]
  final case class Divide[A](exp1: A, exp2: A) extends Exp[A]
  final case class Square[A](exp: A) extends Exp[A]

  type Algebra[F[_], A] = F[A] => A

  type CoAlgebra[F[_], A] = A => F[A]

  implicit val functor: Functor[Exp] = new Functor[Exp] {
    def map[A, B](exp: Exp[A])(f: A => B): Exp[B] = exp match {
      case Sum(a1, a2)      => Sum(f(a1), f(a2))
      case Multiply(a1, a2) => Multiply(f(a1), f(a2))
      case Divide(a1, a2)   => Divide(f(a1), f(a2))
      case Square(a)        => Square(f(a))
      case IntValue(v)      => IntValue(v)
      case DecValue(v)      => DecValue(v)
    }
  }

  // Exp[Double] => Double
  val evaluate: Algebra[Exp, Double] = {
    case IntValue(v)      => v.toDouble
    case DecValue(v)      => v
    case Sum(a1, a2)      => a1 + a2
    case Multiply(a1, a2) => a1 * a2
    case Square(a)        => a * a
    case Divide(a1, a2)   => a1 / a2
  }

  // Exp[Double] => String
  val mkStr: Algebra[Exp, String] = {
    case IntValue(v)      => v.toString
    case DecValue(v)      => v.toString
    case Sum(a1, a2)      => s"($a1 + $a2)"
    case Multiply(a1, a2) => s"($a1 + $a2)"
    case Square(a)        => s"($a)^2"
    case Divide(a1, a2)   => s"($a1 + $a2)"
  }

  val divisors: CoAlgebra[Exp, Int] = {
    case n if n % 2 == 0 && n != 2 => Multiply(2, n / 2)
    case n => IntValue(n)
  }
}