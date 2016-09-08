package model.dailyFinancialParameters

import model.yearlyFinancialParameters.CompanyYearlyFinDataEntry

import scala.collection.immutable.TreeSet
import utils.ordered.OrderedSyntax.{OrderedCompanyDailyFinDataEntry, OrderedDateTime}

case class CompanyDailyFinParameter(symbol: String,
                                    oldestEntryO: Option[CompanyDailyFinDataEntry],
                                    latestEntryO: Option[CompanyDailyFinDataEntry],
                                    allCompanyEntriesOfOneDailyParam: List[CompanyDailyFinDataEntry],
                                    groupedByYearM: Map[Int, List[Double]]
                              ) {

  //TODO: Get filePath in other way (from config)


  /**
    * Entry can be added only if it has the symbol of the current company.
    * The new entry is added by contructing a new CompanyDailyFinData,
    * with entry prepended to allCompanyEntriesOfOneDailyParam.
    * The oldestDividend and earliestDividend are updated appropriately.
    *
    * @param entry: DailyFinDataEntry
    * @return
    */
  def addEntry(entry: CompanyDailyFinDataEntry): CompanyDailyFinParameter = {

    if (entry.symbol == symbol) {
      val year = entry.date.getYear

      val newOldest: CompanyDailyFinDataEntry = oldestEntryO.map {
        oldestEntry =>
          if (oldestEntry.date <= entry.date)
            oldestEntry
          else
            entry
      }.getOrElse(entry)


      val newLatest: CompanyDailyFinDataEntry = latestEntryO.map {
        latestEntry =>
          if (latestEntry.date >= entry.date)
            latestEntry
          else
            entry
      }.getOrElse(entry)

      CompanyDailyFinParameter(symbol, Some(newOldest), Some(newLatest), entry :: allCompanyEntriesOfOneDailyParam,
        groupedByYearM + (year -> (entry.value :: groupedByYearM.get(year).getOrElse(List.empty[Double])))
      )
    }
    else {
      println("CompanyDailyFinParameter.addEntry(): Entry meant for wrong company. Cannot be added")
      this
    }
  }


  def addEntries(entries: List[CompanyDailyFinDataEntry]): CompanyDailyFinParameter = entries match {
    case Nil => this
    case h :: t => addEntry(h).addEntries(t)
  }
  /*def eraseDividend(divi: DailyFinData) {
    CompanyDividend(symbol, oldestDividend, earliestDividend, avgPerYearDividend, allCompanyDividends. - divi)
  }*/

}



object CompanyDailyFinParameter {
  def apply(sym: String): CompanyDailyFinParameter = {
     apply(sym, None, None, Nil, Map.empty)
  }
  def validateValueInLine(index: Int)(line: List[String]): Boolean = {
    line(index).toDouble != Double.NaN && line(index) != null
  }

  def getAvgPerYear(list: List[Double]): Double = list match {
    case Nil => 0
    case h :: t => list.fold(0d)(_ + _) / list.size
  }
}
