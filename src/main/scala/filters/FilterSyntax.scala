package filters

import org.joda.time.DateTime

object FilterSyntax {
  implicit class FilterOps[A](value: A) {
    def filter(consistentYears: Set[Int])(implicit filter: FilterParameterGivenYears[A]) = {
      filter.applyFilter(value, consistentYears)
    }
    def filter(consistentDates: Set[DateTime])(implicit filter: FilterParameterGivenDates[A]) = {
      filter.applyFilter(value, consistentDates)
    }
    def filter(implicit filter: FilterData[A]) = {
      filter.applyFilter(value)
    }
  }

}
