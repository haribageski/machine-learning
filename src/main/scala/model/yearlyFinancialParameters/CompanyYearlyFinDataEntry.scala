package model.yearlyFinancialParameters

import model.SymYear

/**
  *
  * Consists of year: Integer, value: Double, symbol: String
  */
case class CompanyYearlyFinDataEntry(value: Double, year: Int)  {

  def setYear(y: Int): CompanyYearlyFinDataEntry = {
    CompanyYearlyFinDataEntry(value, y)
  }

  def setVal(v: Double): CompanyYearlyFinDataEntry = {
    CompanyYearlyFinDataEntry(v, year)
  }


  override def equals(other: Any) = other match {
    case that: CompanyYearlyFinDataEntry =>
      that.year == year
    case _ => false
  }
}
