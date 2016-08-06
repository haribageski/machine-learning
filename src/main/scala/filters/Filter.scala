package filters

import org.joda.time.DateTime

trait FilterParameterGivenYears[A] {
  def applyFilter(value: A, consistentYears: Set[Int]): A
}

trait FilterParameterGivenDates[A] {
  def applyFilter(value: A, consistentYears: Set[DateTime]): A
}

trait FilterData[A] {
  def applyFilter(value: A): A
}
