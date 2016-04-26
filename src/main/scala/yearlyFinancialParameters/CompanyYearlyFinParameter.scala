package yearlyFinancialParameters

import utils.SymYear

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.language.implicitConversions


case class CompanyYearlyFinParameter(symbol: String,
                                     oldestEntryOpt: Option[YearlyFinDataEntry],
                                     earliestEntryOpt: Option[YearlyFinDataEntry],
                                     perYearM: Map[SymYear, YearlyFinDataEntry],
                                     allCompanyEntriesOfOneYearlyParam: List[YearlyFinDataEntry]
                                       ) {

  /**
    * It simultaneously updates the oldest and newest entry, adds instance to the map, and adds the instance to a list.
    * @param entry: YearlyFinDataEntry
    * @return
    */
  def addEntry(entry: YearlyFinDataEntry): CompanyYearlyFinParameter = {
    import utils.ordered.DefaultOrdered.OrderedSymYear
    if (entry.symbol == symbol) {
      val symYear = SymYear(symbol, entry.year)

      allCompanyEntriesOfOneYearlyParam match {
        case Nil => //this is the case when we add the first dividend
          CompanyYearlyFinParameter(symbol, Some(entry), Some(entry), TreeMap(symYear -> entry), List(entry))

        case l: List[YearlyFinDataEntry] =>
          if (entry.year > oldestEntryOpt.get.year && entry.year < earliestEntryOpt.get.year) {
            CompanyYearlyFinParameter(symbol, oldestEntryOpt, earliestEntryOpt, perYearM + (symYear -> entry), entry :: l)
          }
          else if (entry.year < oldestEntryOpt.get.year && entry.year < earliestEntryOpt.get.year) {
            CompanyYearlyFinParameter(symbol, Some(entry), earliestEntryOpt, perYearM + (symYear -> entry), entry :: l)
          }
          else if (entry.year > oldestEntryOpt.get.year && entry.year > earliestEntryOpt.get.year) {
            CompanyYearlyFinParameter(symbol, oldestEntryOpt, Some(entry), perYearM + (symYear -> entry), entry :: l)
          }
          else {
            CompanyYearlyFinParameter(symbol, Some(entry), Some(entry), perYearM + (symYear -> entry), entry :: l)
          }
      }
    }
    else {
      println("Entry meant for wrong company. Cannot be added")
      this
    }
  }


  @tailrec
  final def addEntries(entriesL: List[YearlyFinDataEntry]): CompanyYearlyFinParameter = entriesL match {
    case Nil => this
    case h :: t => this.addEntry(h).addEntries(t)
  }



  override def toString =
    s"symbol: $symbol, oldest: $oldestEntryOpt, earliest: $earliestEntryOpt, all entries: $allCompanyEntriesOfOneYearlyParam"
}


object CompanyYearlyFinParameter {
  def apply(sym: String): CompanyYearlyFinParameter = {
    apply(sym, None, None, Map.empty, Nil)
  }
}
