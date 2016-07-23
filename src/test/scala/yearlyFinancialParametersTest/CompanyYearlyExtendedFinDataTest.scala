package yearlyFinancialParametersTest

import dailyFinancialParameters.{CompanyDailyFinData, CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import utils.ordered.OrderedSyntax._
import org.scalatest.{FlatSpec, Matchers}
import utils.{DateExtended, SymYear}
import utils.filters.DefaultFilters.CompanyYearlyExtendedFinDataFilter
import utils.readers.ReadableDefaults.CompanyDailyFinDataReader
import utils.readers.ReadableDefaults.CompanyYearlyFinDataReader.readDataFromFile
import yearlyFinancialParameters.{CompanyYearlyExtendedFinData, CompanyYearlyFinData, CompanyYearlyFinDataEntry, CompanyYearlyFinParameter}

import scala.collection.immutable.{TreeMap, TreeSet}
import utils.readers.ReadableDefaults.CompanyYearlyFinDataReader._

class CompanyYearlyExtendedFinDataTest  extends FlatSpec with Matchers {

  val sym = "Example"
  val companyYearlyExtendedFinData = CompanyYearlyExtendedFinData(
    readDataFromFile(sym),
    CompanyDailyFinDataReader.readDataFromFile(sym)
  )
  println("companyYearlyExtendedFinData:" + companyYearlyExtendedFinData)
  val companyYearlyExtendedWithDerivedParams: CompanyYearlyExtendedFinData =
    companyYearlyExtendedFinData.deriveAdditionalFinParameters()

  val entry1 = CompanyYearlyFinDataEntry(sym, 100000, 2014)
  val marketVal = CompanyYearlyFinParameter(
    sym, Some(entry1), Some(entry1), TreeMap(SymYear(sym, 2014) -> entry1), List(entry1)
  )
  val entry2 = CompanyYearlyFinDataEntry(sym, -4, 2014)
  val bMratio = CompanyYearlyFinParameter(
    sym, Some(entry2), Some(entry2), TreeMap(SymYear(sym, 2014) -> entry2), List(entry2)
  )
  val entry3 = CompanyYearlyFinDataEntry(sym, 5, 2014)
  val sizeP = CompanyYearlyFinParameter(
    sym, Some(entry3), Some(entry3), TreeMap(SymYear(sym, 2014) -> entry3), List(entry3)
  )

  val companyYearlyExtendedFinDataWithAllParams = CompanyYearlyExtendedFinData(
    readDataFromFile(sym),
    CompanyDailyFinDataReader.readDataFromFile(sym),
    Some(marketVal),
    Some(bMratio),
    Some(sizeP)
  )

  "deriveAdditionalFinParameters()" should "derive MarketVal, BMratio, and Size" in {
    //three derived parameters
    companyYearlyExtendedWithDerivedParams.companyMarketValues.foreach(_ should be(marketVal))
    companyYearlyExtendedWithDerivedParams.companyBMratio.foreach(_ should be(bMratio))
    companyYearlyExtendedWithDerivedParams.companySize.foreach(_ should be(sizeP))
  }


  "filterInconsistentEntries()" should "filter out inconsistent entries" in {
    import utils.filters.DefaultFilters.CompanyYearlyExtendedFinDataFilter
    import utils.filters.FilterSyntax.FilterOps
    val filtered: CompanyYearlyExtendedFinData = companyYearlyExtendedWithDerivedParams.filter

    val divi1 = CompanyDailyFinDataEntry(sym, 0.01, DateExtended("30/03/2014"))
    val divi2 = CompanyDailyFinDataEntry(sym, 0.02, DateExtended("09/04/2014"))
    val dividend = CompanyDailyFinParameter(
      sym,
      Some(divi1),
      Some(divi2),
      List(divi2, divi1),
      Map(2014 -> TreeSet(divi1, divi2))
    )

    val quote1 = CompanyDailyFinDataEntry(sym, 18, DateExtended("04/01/2014"))
    val quote2 = CompanyDailyFinDataEntry(sym, 22, DateExtended("04/04/2014"))
    val quote = CompanyDailyFinParameter(
      sym,
      Some(quote1),
      Some(quote2),
      List(quote2, quote1),
      Map(2014 -> TreeSet(quote1, quote2))
    )

    val sue1 = CompanyDailyFinDataEntry(sym, -1.12000000476837, DateExtended("17/11/2014"))
    val sue2 = CompanyDailyFinDataEntry(sym, 1.51999998092651, DateExtended("13/02/2014"))
    val sue = CompanyDailyFinParameter(
      sym,
      Some(sue2),
      Some(sue1),
      List(sue1, sue2),
      Map(2014 -> TreeSet(sue1, sue2))
    )

    filtered.companyMarketValues.foreach(_ should be(marketVal))
    filtered.companyBMratio.foreach(_ should be(bMratio))
    filtered.companySize.foreach(_ should be(sizeP))
    filtered.companyDailyFinData.parameterDividends should be(dividend)
    filtered.companyDailyFinData.parameterQuotes should be(quote)
    filtered.companyDailyFinData.parameterSUEs should be(sue)
  }
}
