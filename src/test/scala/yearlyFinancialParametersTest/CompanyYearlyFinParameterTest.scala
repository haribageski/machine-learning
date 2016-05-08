package yearlyFinancialParametersTest

import org.scalatest.{FlatSpec, Matchers}
import utils.SymYear
import yearlyFinancialParameters.{CompanyYearlyFinParameter, CompanyYearlyFinDataEntry}
import utils.ordered.OrderedSyntax._
import scala.collection.immutable.TreeMap


class CompanyYearlyFinParameterTest extends FlatSpec with Matchers {
  "addEntry() " should "update the CompanyYearlyFinParameter by constructing a new CompanyYearlyFinParameter" in {
    val companyYearlyFinParameter1: CompanyYearlyFinParameter = CompanyYearlyFinParameter(
        "A" , null, null, TreeMap.empty[SymYear, CompanyYearlyFinDataEntry], List.empty[CompanyYearlyFinDataEntry]
    )
    val symYear1 = SymYear("A", 2015)
    val entry1 = CompanyYearlyFinDataEntry("A", 124.2, 2015)
    val companyYearlyFinParameter2 = CompanyYearlyFinParameter(
        "A", Some(entry1), Some(entry1), TreeMap(symYear1 -> entry1), List(entry1)
    )

    companyYearlyFinParameter1.addEntry(entry1) should be(companyYearlyFinParameter2)

    val symYear2 = SymYear("A", 2014)
    val entry2 = CompanyYearlyFinDataEntry("A", 134.2, 2014)
    val companyYearlyFinParameter3 = CompanyYearlyFinParameter(
        "A", Some(entry2), Some(entry1), TreeMap(symYear2 -> entry2, symYear1 -> entry1), List(entry2, entry1)
    )
    companyYearlyFinParameter2.addEntry(entry2) should be(companyYearlyFinParameter3)
  }
}
