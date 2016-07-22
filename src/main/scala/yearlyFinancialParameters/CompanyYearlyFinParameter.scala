package yearlyFinancialParameters

import utils.SymYear

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.language.implicitConversions


case class CompanyYearlyFinParameter(symbol: String,
                                     oldestEntryOpt: Option[CompanyYearlyFinDataEntry],
                                     earliestEntryOpt: Option[CompanyYearlyFinDataEntry],
                                     perYearM: Map[SymYear, CompanyYearlyFinDataEntry],
                                     allCompanyEntriesOfOneYearlyParam: List[CompanyYearlyFinDataEntry]
                                       ) {

  /**
    * It simultaneously updates the oldest and newest entry, adds instance to the map, and adds the instance to a list.
    *
    * @param entry: YearlyFinDataEntry
    * @return
    */
  def addEntry(entry: CompanyYearlyFinDataEntry): CompanyYearlyFinParameter = {
    import utils.ordered.OrderedSyntax.OrderedSymYear
    if (entry.symbol == symbol) {
      val symYear = SymYear(symbol, entry.year)

      allCompanyEntriesOfOneYearlyParam match {
        case Nil => //this is the case when we add the first dividend
          CompanyYearlyFinParameter(symbol, Some(entry), Some(entry), TreeMap(symYear -> entry), List(entry))

        case l: List[CompanyYearlyFinDataEntry] =>
          if (oldestEntryOpt.forall(_.year <= entry.year) && earliestEntryOpt.forall(_.year >= entry.year)) {
            this.copy(
              perYearM = perYearM + (symYear -> entry),
              allCompanyEntriesOfOneYearlyParam = entry :: l
            )
          }
          else if (oldestEntryOpt.forall(_.year >= entry.year) && earliestEntryOpt.forall(_.year >= entry.year )) {
            this.copy(
              oldestEntryOpt = Some(entry),
              perYearM = perYearM + (symYear -> entry),
              allCompanyEntriesOfOneYearlyParam = entry :: l
            )
          }
          else if (oldestEntryOpt.forall(_.year <= entry.year) && earliestEntryOpt.forall(_.year <= entry.year)) {
            this.copy(
              earliestEntryOpt = Some(entry),
              perYearM = perYearM + (symYear -> entry),
              allCompanyEntriesOfOneYearlyParam = entry :: l
            )
          }
          else {
            this.copy(
              oldestEntryOpt = Some(entry),
              earliestEntryOpt = Some(entry),
              perYearM = perYearM + (symYear -> entry),
              allCompanyEntriesOfOneYearlyParam = entry :: l
            )
          }
      }
    }
    else {
      println("Entry meant for wrong company. Cannot be added")
      this
    }
  }


  @tailrec
  final def addEntries(entriesL: List[CompanyYearlyFinDataEntry]): CompanyYearlyFinParameter = entriesL match {
    case Nil => this
    case h :: t => this.addEntry(h).addEntries(t)
  }



//  override def toString =
//    s"symbol: $symbol, oldest: $oldestEntryOpt, earliest: $earliestEntryOpt, all entries: $allCompanyEntriesOfOneYearlyParam"
}


object CompanyYearlyFinParameter {
  def apply(sym: String): CompanyYearlyFinParameter = {
    apply(sym, None, None, Map.empty, Nil)
  }
}
