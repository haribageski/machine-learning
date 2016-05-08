package yearlyFinancialParameters

import utils.SymYear

/**
  *
  * Consists of year: Integer, value: Double, symbol: String
  */
case class CompanyYearlyFinDataEntry(symbol: String, value: Double, year: Int)  {

  def setYear(y: Int): CompanyYearlyFinDataEntry = {
    CompanyYearlyFinDataEntry(symbol, value, y)
  }

  def setVal(v: Double): CompanyYearlyFinDataEntry = {
    CompanyYearlyFinDataEntry(symbol, v, year)
  }

  def canEqual(other: Any): Boolean = {
    other.isInstanceOf[CompanyYearlyFinDataEntry]
  }

  override def equals(other: Any) = other match {
    case that: CompanyYearlyFinDataEntry =>
      canEqual(other) && (that.symbol == symbol) && (that.value == value) && (that.year == year)
    case _ => false
  }
//
//  override def compare(that: CompanyYearlyFinDataEntry) = {
//    //System.out.println(s"Comparing $value with ${that.value}:" + value.compareTo(that.value))
//    value.compareTo(that.value)
//  }

  val symYear = SymYear(symbol, year)
}
