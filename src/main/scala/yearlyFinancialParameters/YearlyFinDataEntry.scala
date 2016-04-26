package yearlyFinancialParameters

import utils.SymYear

/**
  *
  * Consists of year: Integer, value: Double, symbol: String
  */
case class YearlyFinDataEntry(symbol: String, value: Double, year: Int) extends Ordered[YearlyFinDataEntry] {

  def setYear(y: Int): YearlyFinDataEntry = {
    YearlyFinDataEntry(symbol, value, y)
  }

  def setVal(v: Double): YearlyFinDataEntry = {
    YearlyFinDataEntry(symbol, v, year)
  }

  def canEqual(other: Any): Boolean = {
    other.isInstanceOf[YearlyFinDataEntry]
  }

  override def equals(other: Any) = other match {
    case that: YearlyFinDataEntry =>
      canEqual(other) && (that.symbol == symbol) && (that.value == value) && (that.year == year)
    case _ => false
  }

  override def compare(that: YearlyFinDataEntry) = {
    //System.out.println(s"Comparing $value with ${that.value}:" + value.compareTo(that.value))
    value.compareTo(that.value)
  }

  val symYear = SymYear(symbol, year)
}
