package model.dailyFinancialParameters

import utils.DateExtended
import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import utils.ordered.OrderedSyntax.{OrderedCompanyDailyFinDataEntry, OrderedDateExtended}

case class CompanyDailyFinParameter(symbol: String,
                                    oldestEntryO: Option[CompanyDailyFinDataEntry],
                                    earliestEntryO: Option[CompanyDailyFinDataEntry],
                                    allCompanyEntriesOfOneDailyParam: List[CompanyDailyFinDataEntry],
                                    groupedByYearM: Map[Int, TreeSet[CompanyDailyFinDataEntry]]
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
      val year = entry.date.dateExtended.getYear
      val mapValueTotalPerYear: TreeSet[CompanyDailyFinDataEntry] = groupedByYearM.getOrElse(year, TreeSet())

      allCompanyEntriesOfOneDailyParam match {
        //this is the case when we add the first dividend
        case Nil =>
          new CompanyDailyFinParameter(symbol, Some(entry), Some(entry), List(entry), Map(year -> TreeSet(entry)))

        case l: List[CompanyDailyFinDataEntry] =>
          if (oldestEntryO.forall(_.date <= entry.date) && earliestEntryO.forall(_.date >= entry.date))
            this.copy(
              allCompanyEntriesOfOneDailyParam = entry :: l,
              groupedByYearM = groupedByYearM + (year -> (mapValueTotalPerYear + entry))
            )
          else if (oldestEntryO.forall(_.date >= entry.date))
            this.copy(
              oldestEntryO = Some(entry),
              allCompanyEntriesOfOneDailyParam = entry :: l,
              groupedByYearM = groupedByYearM + (year -> (mapValueTotalPerYear + entry))
            )
          else //if (earliestEntryO.forall(_.date <= entry.date))
            this.copy(
              earliestEntryO = Some(entry),
              allCompanyEntriesOfOneDailyParam = entry :: l,
              groupedByYearM = groupedByYearM + (year -> (mapValueTotalPerYear + entry))
            )
//          else
//            this.copy(
//              oldestEntryO = Some(entry),
//              earliestEntryO = Some(entry),
//              allCompanyEntriesOfOneDailyParam = entry :: l,
//              groupedByYearM = groupedByYearM + (year -> (mapValueTotalPerYear + entry))
//            )
      }
    }
    else {
      println("Entry meant for wrong company. Cannot be added")
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

}
