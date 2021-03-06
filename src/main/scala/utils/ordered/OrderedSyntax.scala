package utils.ordered

import model.{DateExtended, SymDate, SymYear}
import model.dailyFinancialParameters.CompanyDailyFinDataEntry
import model.yearlyFinancialParameters.CompanyYearlyFinDataEntry
import org.joda.time.DateTime

/**
  * Set of interface-classes that wrap a class and make it Ordered.
  */
object OrderedSyntax {

  implicit class OrderedDateExtended(dateExtended: DateExtended) extends Ordered[DateExtended] {
    override def compare(other: DateExtended): Int = {
      dateExtended.dateExtended.compareTo(other.dateExtended)
    }
  }

  implicit class OrderedDateTime(dateTime: DateTime) extends Ordered[DateTime] {
    override def compare(other: DateTime): Int = {
      dateTime.compareTo(other)
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
      if(companyDailyFinDataEntry.symbol != that.symbol)
        companyDailyFinDataEntry.symbol.compareTo(that.symbol)
      else
        companyDailyFinDataEntry.date.compareTo(that.date)
    }
  }

  implicit class OrderedCompanyYearlyFinDataEntry(companyYearlyFinDataEntry: CompanyYearlyFinDataEntry)
    extends Ordered[CompanyYearlyFinDataEntry] {
    def compare(that: CompanyYearlyFinDataEntry) =
      companyYearlyFinDataEntry.year.compareTo(that.year)
  }
}
