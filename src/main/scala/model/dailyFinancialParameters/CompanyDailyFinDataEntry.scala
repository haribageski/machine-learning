package model.dailyFinancialParameters

import org.joda.time.DateTime

case class CompanyDailyFinDataEntry(symbol: String, value: Double, date: DateTime) {
  def setDate(d: DateTime): CompanyDailyFinDataEntry = {
    copy(date = d)
  }

  def setVal(v: Double): CompanyDailyFinDataEntry = {
    copy(value = v)
  }

  override def equals(other: Any) = other match {
    case that: CompanyDailyFinDataEntry =>
      that.symbol == symbol && that.date == date
    case _ => false
  }
}
