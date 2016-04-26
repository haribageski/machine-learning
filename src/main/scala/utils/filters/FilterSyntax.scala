package utils.filters

object FilterSyntax {
  implicit class FilterOps[A](value: A) {
    def filter(implicit filter: FilterParameter[A], consistentYears: Set[Int]) = {
      filter.applyFilter(value, consistentYears)
    }

    def filter(implicit filter: FilterData[A]) = {
      filter.applyFilter(value)
    }
  }

}
