package dailyFinancialParameters

import org.scalatest.{Matchers, FlatSpec}
import utils.DateExtended
import utils.ordered.OrderedSyntax._
import scala.collection.immutable.TreeSet

class CompanyDailyFinParameterTest extends FlatSpec with Matchers {
  //synchronizeEntries() already tested indirectly in ParameterDividendsTes
  "addEntry() " should "update the CompanyDividends by constructing a new CompanyDailyFinParameter" in {

    val companyDividend1 = CompanyDailyFinParameter("A")
    val dividend1 = CompanyDailyFinDataEntry("A", 1000, DateExtended("01/01/2015"))
    val companyDividend2 =
      CompanyDailyFinParameter("A", Some(dividend1), Some(dividend1), List(dividend1),
        Map(2015 -> TreeSet(dividend1))
      )

    companyDividend1.addEntry(dividend1) == companyDividend2 should be(true)

    val dividend2 = CompanyDailyFinDataEntry("A", 1000, DateExtended("01/02/2015"))
    val companyDividend3 =
      CompanyDailyFinParameter("A", Some(dividend1), Some(dividend2), List(dividend2, dividend1),
        Map(2015 -> TreeSet(dividend2, dividend1))
      )
    companyDividend2.addEntry(dividend2) == companyDividend3 should be(true)
  }
}
