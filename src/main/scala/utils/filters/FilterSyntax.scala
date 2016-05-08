package utils.filters

object FilterSyntax {
  implicit class FilterOps[A](value: A) {
    def filter(consistentYears: Set[Int])(implicit filter: FilterParameter[A]) = {
      filter.applyFilter(value, consistentYears)
    }
    def filter(implicit filter: FilterData[A]) = {
      filter.applyFilter(value)
    }
  }

}
