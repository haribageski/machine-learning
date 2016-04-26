package utils.cats_Show

import cats.Show

object ShowSyntax {
  implicit class ShowSyntax[A](value: A) {
    def show(implicit valueShow: Show[A]): String = valueShow.show(value)

    def outputString(implicit valueShow: Show[A]): Unit = println(valueShow.show(value))
  }
}
