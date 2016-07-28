package model.yearlyFinancialParameters

import model.SymYear

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

  val symYear = SymYear(symbol, year)

  override def equals(other: Any) = other match {
    case that: CompanyYearlyFinDataEntry =>
      that.symbol == symbol && that.year == year
    case _ => false
  }
}
