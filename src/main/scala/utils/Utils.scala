package utils

object Utils {
  def foldr[A, B]( combine: (A, =>B) => B, base: B )(xs: Stream[A]): B = {
    if (xs.isEmpty)
      base
    else
      combine(xs.head,  foldr(combine, base)(xs.tail))
  }
}
