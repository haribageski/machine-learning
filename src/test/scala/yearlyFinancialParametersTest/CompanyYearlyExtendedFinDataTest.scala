package yearlyFinancialParametersTest

import dailyFinancialParameters.CompanyDailyFinData
import utils.ordered.OrderedSyntax.OrderedSymYear
import org.scalatest.{FlatSpec, Matchers}
import utils.SymYear
import utils.readers.ReadableDefaults.CompanyDailyFinDataReader
import yearlyFinancialParameters.{CompanyYearlyExtendedFinData, CompanyYearlyFinData, CompanyYearlyFinDataEntry, CompanyYearlyFinParameter}

import scala.collection.immutable.TreeMap

class CompanyYearlyExtendedFinDataTest  extends FlatSpec with Matchers {
  "deriveAdditionalFinParameters() and filterInconsistentEntries()" should "derive MarketVal, BMratio, and Size" in {

    import utils.filters.DefaultFilters.CompanyYearlyExtendedFinDataFilter
    import utils.filters.FilterSyntax.FilterOps
    import utils.readers.ReadableDefaults.CompanyYearlyFinDataReader._

    val sym = "Example"
    val companyYearlyExtendedFinData = CompanyYearlyExtendedFinData(
      readDataFromFile(sym),
      CompanyDailyFinDataReader.readDataFromFile(sym)
    )
    val companyYearlyExtendedWithDerivedParams: CompanyYearlyExtendedFinData =
      companyYearlyExtendedFinData.deriveAdditionalFinParameters()

    //printing the three derived parameters
    companyYearlyExtendedWithDerivedParams.companyMarketValues.foreach(m =>
      m.allCompanyEntriesOfOneYearlyParam.head
        should be(CompanyYearlyFinDataEntry(sym, 100000, 2014)
      )
    )
    companyYearlyExtendedWithDerivedParams.companyBMratio.foreach(m =>
      m.allCompanyEntriesOfOneYearlyParam.head
        should be(CompanyYearlyFinDataEntry(sym, -4, 2014)
      )
    )
    companyYearlyExtendedWithDerivedParams.companySize.foreach(m =>
      m.allCompanyEntriesOfOneYearlyParam.head
        should be(CompanyYearlyFinDataEntry(sym, 5, 2014)
      )
    )

    val entry1 = CompanyYearlyFinDataEntry("A", 10, 2014)
    val marketVal = CompanyYearlyFinParameter(
      "A", Some(entry1), Some(entry1), TreeMap(SymYear("A", 2014) -> entry1), List(entry1)
    )
    val entry2 = CompanyYearlyFinDataEntry("A", 5000, 2014)
    val bMratio = CompanyYearlyFinParameter(
      "A", Some(entry2), Some(entry2), TreeMap(SymYear("A", 2014) -> entry2), List(entry2)
    )
    val entry3 = CompanyYearlyFinDataEntry("A", 1.12999999523163, 2014)
    val size = CompanyYearlyFinParameter(
      "A", Some(entry3), Some(entry3), TreeMap(SymYear("A", 2014) -> entry3), List(entry3)
    )

    val companyYearlyExtendedFinDataWithAllParamsFilled = CompanyYearlyExtendedFinData(
      readDataFromFile(sym),
      CompanyDailyFinDataReader.readDataFromFile(sym),
      Some(marketVal),
      Some(bMratio),
      Some(size)
    )
    val companyYearlyExtendedFromFileAndManual: CompanyYearlyExtendedFinData =
      companyYearlyExtendedFinDataWithAllParamsFilled.deriveAdditionalFinParameters()

    //printing the three derived parameters
    companyYearlyExtendedFromFileAndManual.companyMarketValues.foreach(m =>
      m.allCompanyEntriesOfOneYearlyParam.head
        should be (entry1)
    )
    companyYearlyExtendedFromFileAndManual.companyBMratio.foreach(m =>
      m.allCompanyEntriesOfOneYearlyParam.head
        should be (entry2)
    )
    companyYearlyExtendedFromFileAndManual.companySize.foreach(m =>
      m.allCompanyEntriesOfOneYearlyParam.head
        should be(entry3)
    )


    val filteredcompanyYearlyExtendedWithDerivedParams: CompanyYearlyExtendedFinData =
      companyYearlyExtendedWithDerivedParams.filter(CompanyYearlyExtendedFinDataFilter)
    filteredcompanyYearlyExtendedWithDerivedParams.companyBMratio.forall(
      _.perYearM(SymYear(sym, 2014)).year == 2014)  should be (true)
    filteredcompanyYearlyExtendedWithDerivedParams.companyMarketValues.forall(
      _.perYearM(SymYear(sym, 2014)).year == 2014)  should be (true)
    filteredcompanyYearlyExtendedWithDerivedParams.companySize.forall(
      _.perYearM(SymYear(sym, 2014)).year == 2014)  should be (true)
    filteredcompanyYearlyExtendedWithDerivedParams.companyMarketValues.forall(
      _.perYearM(SymYear(sym, 2014)).year == 2014)  should be (true)

    filteredcompanyYearlyExtendedWithDerivedParams
      .companyYearlyFinData
      .accrual
      .allCompanyEntriesOfOneYearlyParam
      .forall(_.year == 2014) should be (true)
    filteredcompanyYearlyExtendedWithDerivedParams
      .companyYearlyFinData
      .bookValue
      .allCompanyEntriesOfOneYearlyParam
      .forall(_.year == 2014) should be (true)
    filteredcompanyYearlyExtendedWithDerivedParams
      .companyYearlyFinData
      .rOE
      .allCompanyEntriesOfOneYearlyParam
      .forall(_.year == 2014) should be (true)
    filteredcompanyYearlyExtendedWithDerivedParams
      .companyYearlyFinData
      .shares
      .allCompanyEntriesOfOneYearlyParam
      .forall(_.year == 2014) should be (true)

    1 should be (1)   //TODO: Do appropriate testing
  }
}
