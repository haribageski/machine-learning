package dailyFinancialParameters

import utils.DateExtended

case class CompanyDailyFinDataEntry(symbol: String, value: Double, date: DateExtended)  {
  def setDate(d: DateExtended): CompanyDailyFinDataEntry = {
    CompanyDailyFinDataEntry(symbol, value, d)
  }

  def setVal(v: Double): CompanyDailyFinDataEntry = {
    CompanyDailyFinDataEntry(symbol, v, date)
  }

//  def canEqual(other: Any): Boolean = {
//    other.isInstanceOf[CompanyDailyFinDataEntry]
//  }

//  override def equals(other: Any) = other match {
//    case that: CompanyDailyFinDataEntry =>
//      canEqual(other) && (that.symbol == symbol) && (that.value == value) && (that.date == date)
//    case _ => false
//  }
}
