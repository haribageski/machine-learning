package yearlyFinancialParametersTest

import dailyFinancialParameters.CompanyDailyFinData
import org.scalatest.{Matchers, FlatSpec}
import utils.SymYear
import utils.readers.ReadableDefaults.CompanyDailyFinDataReader
import yearlyFinancialParameters.{YearlyFinDataEntry, CompanyYearlyFinData, CompanyYearlyExtendedFinData}

class CompanyYearlyExtendedFinDataTest  extends FlatSpec with Matchers {
  "deriveAdditionalFinParameters() and filterInconsistentEntries()" should "derive MarketVal, BMratio, and Size" in {

    import utils.filters.DefaultFilters.CompanyYearlyExtendedFinDataFilter
    import utils.filters.FilterSyntax.FilterOps

    val sym = "Example"
    val companyYearlyExtendedFinData = CompanyYearlyExtendedFinData(
      CompanyYearlyFinData(sym).readFromFiles(),
      CompanyDailyFinDataReader.readDataFromFile(sym)
    )
    val companyYearlyExtendedWithDerivedParams: CompanyYearlyExtendedFinData =
      companyYearlyExtendedFinData.deriveAdditionalFinParameters()

    //printing the three derived parameters
    companyYearlyExtendedWithDerivedParams.companyMarketValues.foreach(m =>
      m.allCompanyEntriesOfOneYearlyParam.head
        should be (YearlyFinDataEntry(sym, 100000, 2014)
      )
    )
    companyYearlyExtendedWithDerivedParams.companyBMratio.foreach(m =>
      m.allCompanyEntriesOfOneYearlyParam.head
    should be (YearlyFinDataEntry(sym, -4, 2014)
      )
    )
    companyYearlyExtendedWithDerivedParams.companySize.foreach(m =>
      m.allCompanyEntriesOfOneYearlyParam.head
        should be (YearlyFinDataEntry(sym, 5, 2014)
      )
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
