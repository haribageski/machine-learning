package model.dailyFinancialParameters

import model.DateExtended
import org.scalatest.{FlatSpec, Matchers}
import utils.ordered.OrderedSyntax._
import scala.collection.immutable.TreeSet

class CompanyDailyFinParameterSpec extends FlatSpec with Matchers {
  //synchronizeEntries() already tested indirectly in ParameterDividendsTes
  "addEntry() " should "update the CompanyDividends by constructing a new CompanyDailyFinParameter" in {

    val companyDividend1 = CompanyDailyFinParameter("A")
    val dividend1 = CompanyDailyFinDataEntry("A", 1000, DateExtended.fromString("01/01/2015"))
    val companyDividend2 =
      CompanyDailyFinParameter("A", Some(dividend1), Some(dividend1), List(dividend1),
        Map(2015 -> List(dividend1.value))
      )

    companyDividend1.addEntry(dividend1) == companyDividend2 should be(true)

    val dividend2 = CompanyDailyFinDataEntry("A", 1000, DateExtended.fromString("01/03/2015"))
    val companyDividend3 =
      CompanyDailyFinParameter("A", Some(dividend1), Some(dividend2), List(dividend2, dividend1),
        Map(2015 -> List(dividend2.value, dividend1.value))
      )
    companyDividend2.addEntry(dividend2) == companyDividend3 should be(true)

    val dividend3 = CompanyDailyFinDataEntry("A", 1000, DateExtended.fromString("01/02/2015"))
    val companyDividend4 =
      CompanyDailyFinParameter("A", Some(dividend1), Some(dividend2), List(dividend3, dividend2, dividend1),
        Map(2015 -> List(dividend2.value, dividend3.value, dividend1.value))
      )
    companyDividend3.addEntry(dividend3) == companyDividend4 should be(true)

    val wrongCompanyDivid = CompanyDailyFinDataEntry("B", 1000, DateExtended.fromString("01/02/2015"))
    companyDividend3.addEntry(wrongCompanyDivid) == companyDividend3 should be(true)
  }
}
