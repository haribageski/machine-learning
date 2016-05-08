package utils.ordered

import dailyFinancialParameters.CompanyDailyFinDataEntry
import utils.{DateExtended, SymDate, SymYear}
import yearlyFinancialParameters.CompanyYearlyFinDataEntry

/**
  * Set of interface-classes that wrap a class and make it Ordered.
  */
object OrderedSyntax {

  implicit class OrderedDateExtended(dateExtended: DateExtended) extends Ordered[DateExtended] {
    override def compare(other: DateExtended): Int = {
      dateExtended.dateExtended.compareTo(other.dateExtended)
    }
  }

  implicit class OrderedSymDate(symDate: SymDate) extends Ordered[SymDate] {
    override def compare(other: SymDate): Int = {
      if (other.sym != symDate.sym) symDate.sym.compareTo(other.sym)
      else symDate.dateE.compare(other.dateE)
    }
  }

  implicit class OrderedSymYear(symYear: SymYear) extends Ordered[SymYear] {
    override def compare(other: SymYear): Int = {
      if (other.sym != symYear.sym) symYear.sym.compareTo(other.sym)
      else symYear.year.compareTo(other.year)
    }
  }

  implicit class OrderedCompanyDailyFinDataEntry(companyDailyFinDataEntry: CompanyDailyFinDataEntry)
    extends Ordered[CompanyDailyFinDataEntry] {
    def compare(that: CompanyDailyFinDataEntry) = {
      companyDailyFinDataEntry.value.compareTo(that.value)
    }
  }

  implicit class OrderedCompanyYearlyFinDataEntry(companyYearlyFinDataEntry: CompanyYearlyFinDataEntry)
    extends Ordered[CompanyYearlyFinDataEntry] {
    def compare(that: CompanyYearlyFinDataEntry) = {
      companyYearlyFinDataEntry.value.compareTo(that.value)
    }
  }
}
