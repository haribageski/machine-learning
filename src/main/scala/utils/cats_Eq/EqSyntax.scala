package utils.cats_Eq

import EqDefaults._
import cats.Eq
import cats.syntax.eq._

object EqSyntax {
  implicit class EqSyntax[A](value: A){
    def equals(toCompareWith: A)(implicit comparator: Eq[A]) =
      value === toCompareWith
  }
}
