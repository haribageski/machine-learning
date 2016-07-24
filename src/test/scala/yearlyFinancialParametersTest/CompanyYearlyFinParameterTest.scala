package yearlyFinancialParametersTest

import model.SymYear
import org.scalatest.{FlatSpec, Matchers}
import model.yearlyFinancialParameters.{CompanyYearlyFinDataEntry, CompanyYearlyFinParameter}
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

    val symYear2 = SymYear("A", 2013)
    val entry2 = CompanyYearlyFinDataEntry("A", 134.2, 2013)
    val companyYearlyFinParameter3 = CompanyYearlyFinParameter(
      "A", Some(entry2), Some(entry1), TreeMap(symYear2 -> entry2, symYear1 -> entry1), List(entry2, entry1)
    )
    companyYearlyFinParameter2.addEntry(entry2) should be(companyYearlyFinParameter3)

    val symYear3 = SymYear("A", 2014)
    val entry3 = CompanyYearlyFinDataEntry("A", 134.2, 2014)
    val companyYearlyFinParameter4 = CompanyYearlyFinParameter(
      "A", Some(entry2), Some(entry1), TreeMap(symYear2 -> entry2, symYear3 -> entry3, symYear1 -> entry1),
      List(entry3, entry2, entry1)
    )
    companyYearlyFinParameter3.addEntry(entry3) should be(companyYearlyFinParameter4)

    val wrongEntry = CompanyYearlyFinDataEntry("C", 134.2, 2016)
    companyYearlyFinParameter4.addEntry(wrongEntry) should be(companyYearlyFinParameter4)
  }
}
