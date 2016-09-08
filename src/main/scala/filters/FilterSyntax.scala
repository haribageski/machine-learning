package filters

import org.joda.time.DateTime

object FilterSyntax {
  implicit class FilterOps[A](value: A) {
    def filter(consistentYears: Set[Int])(implicit filter: FilterParameterGivenYears[A]): A = {
      filter.applyFilter(value, consistentYears)
    }
    def filter(consistentDates: Set[DateTime])(implicit filter: FilterParameterGivenDates[A]): A = {
      filter.applyFilter(value, consistentDates)
    }
    def filter(implicit filter: FilterData[A]): Option[A] = {
      filter.applyFilter(value)
    }
  }

}
