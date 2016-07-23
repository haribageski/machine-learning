package dailyFinancialParameters

import utils.DateExtended

case class CompanyDailyFinDataEntry(symbol: String, value: Double, date: DateExtended)  {
  def setDate(d: DateExtended): CompanyDailyFinDataEntry = {
    copy(date = d)
  }

  def setVal(v: Double): CompanyDailyFinDataEntry = {
    copy(value = v)
  }

  override def equals(other: Any) = other match {
    case that: CompanyDailyFinDataEntry =>
      that.symbol == symbol && that.value == value
    case _ => false
  }
}
