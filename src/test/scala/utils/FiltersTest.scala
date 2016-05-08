package utils

import dailyFinancialParameters.{CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import org.scalatest.{FlatSpec, Matchers}
import utils.ordered.OrderedSyntax._
import yearlyFinancialParameters.{CompanyYearlyFinParameter, CompanyYearlyFinDataEntry}
import utils.filters.DefaultFilters._
import utils.filters.FilterSyntax.FilterOps
import scala.collection.immutable.TreeSet

class FiltersTest extends FlatSpec with Matchers {
  "filter()" should
  "return parameter that contains only consistent in year entries" in {
    import utils.readers.ReadableDefaults.CompanyDailyFinParameterReader

    val sym = "EZPW"
    val dividends = List(
      CompanyDailyFinDataEntry(sym, 0.00400000018998981, DateExtended("07/08/1998")),
      CompanyDailyFinDataEntry(sym, 0.00400000018998981, DateExtended("20/11/1998")),
      CompanyDailyFinDataEntry(sym, 0.00432999990880489, DateExtended("04/02/2000"))
    )

    val earliestD = CompanyDailyFinDataEntry(sym, 0.00432999990880489, DateExtended("04/02/2000"))
    val oldestD = CompanyDailyFinDataEntry(sym, 0.00400000018998981, DateExtended("07/08/1998"))

    val toComp = CompanyDailyFinParameter(
      sym, Some(oldestD), Some(earliestD), dividends,
      Map(
        1998 -> TreeSet(
          CompanyDailyFinDataEntry(sym, 0.00400000018998981, DateExtended("07/08/1998")),
          CompanyDailyFinDataEntry(sym, 0.00400000018998981, DateExtended("20/11/1998"))
        ),
        2000 -> TreeSet(
          CompanyDailyFinDataEntry(sym, 0.00432999990880489, DateExtended("04/02/2000"))
        )
      )
    )
    val dividendsRead: CompanyDailyFinParameter = CompanyDailyFinParameterReader.readDividendFromFile(sym)
    val synchronizedDividends: CompanyDailyFinParameter = dividendsRead.filter(Set(1998, 2000))

    println("synchronizedDividends:" + synchronizedDividends)
    println("toComp:" + toComp)
    synchronizedDividends == toComp should be(true)
  }


  "filter" should "return parameter that contains only consistent in year entries" in {

    val companyYearlyFinParameter1: CompanyYearlyFinParameter = CompanyYearlyFinParameter("A")

    val entry1 = CompanyYearlyFinDataEntry("A", 124.2, 2015)
    val companyYearlyFinParameter2 = companyYearlyFinParameter1.addEntry(entry1)

    val entry2 = CompanyYearlyFinDataEntry("A", 134.2, 2014)
    val companyYearlyFinParameter3 = companyYearlyFinParameter2.addEntry(entry2)

    companyYearlyFinParameter3.filter(Set(2015)) should be (companyYearlyFinParameter2)
    companyYearlyFinParameter3.filter(Set.empty[Int]) should be(companyYearlyFinParameter1)
    companyYearlyFinParameter3.filter(Set(2015, 2014)) should be(companyYearlyFinParameter3)
  }
}
