package com.demo.gadt

sealed trait Expr[A]
final case class Num(x: Int) extends Expr[Int]
final case class Bool(x: Boolean) extends Expr[Boolean]
final case class Add(x: Expr[Int], y: Expr[Int]) extends Expr[Int]
final case class Equals[A](x: Expr[A], y: Expr[A]) extends Expr[Boolean]

object MainE extends App {

  def eval[A](expr: Expr[A]): A = expr match {
    case Num(x)       => x
    case Bool(x)      => x
    case Add(x, y)    => eval(x) + eval(y)
    case Equals(x, y) => eval(x) == eval(y)
  }

  val expression: Expr[Boolean] =
    Equals(
      Equals(
        Num(3),
        Add(
          Num(1),
          Num(2))),
      Bool(true))

  println(eval(expression))

}
