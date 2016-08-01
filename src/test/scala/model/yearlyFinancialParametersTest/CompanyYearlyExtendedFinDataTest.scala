package model.yearlyFinancialParametersTest

import filters.{DefaultFilters, FilterSyntax}
import model.{DateExtended, SymYear}
import model.dailyFinancialParameters._
import utils.ordered.OrderedSyntax._
import org.scalatest.{FlatSpec, Matchers}
import DefaultFilters.CompanyYearlyExtendedFinDataFilter
import utils.readers.ReadableDefaults.CompanyDailyFinDataReader
import utils.readers.ReadableDefaults.CompanyYearlyFinDataReader.readDataFromFile
import model.yearlyFinancialParameters.{CompanyYearlyExtendedFinData, CompanyYearlyFinData, CompanyYearlyFinDataEntry, CompanyYearlyFinParameter}
import DefaultFilters.CompanyYearlyExtendedFinDataFilter
import FilterSyntax.FilterOps
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
    val filtered: CompanyYearlyExtendedFinData = companyYearlyExtendedWithDerivedParams.filter

    val divi1 = CompanyDailyFinDataEntry(sym, 0.01, DateExtended.fromString("30/03/2014"))
    val divi2 = CompanyDailyFinDataEntry(sym, 0.02, DateExtended.fromString("09/04/2014"))
    val dividend = CompanyDailyFinParameter(
      sym,
      Some(divi1),
      Some(divi2),
      List(divi2, divi1),
      Map(2014 -> TreeSet(divi1, divi2))
    )

    val quote1 = CompanyDailyFinDataEntry(sym, 18, DateExtended.fromString("04/01/2014"))
    val quote2 = CompanyDailyFinDataEntry(sym, 22, DateExtended.fromString("04/04/2014"))
    val quote = CompanyDailyFinParameter(
      sym,
      Some(quote1),
      Some(quote2),
      List(quote2, quote1),
      Map(2014 -> TreeSet(quote1, quote2))
    )

    val sue4 = CompanyDailyFinDataEntry(sym, 1, DateExtended.fromString("19/11/2014"))
    val sue3 = CompanyDailyFinDataEntry(sym, -2, DateExtended.fromString("18/11/2014"))
    val sue2 = CompanyDailyFinDataEntry(sym, -1.12000000476837, DateExtended.fromString("17/11/2014"))
    val sue1 = CompanyDailyFinDataEntry(sym, 1.51999998092651, DateExtended.fromString("13/02/2014"))
    val sue = CompanyDailyFinParameter(
      sym,
      Some(sue1),
      Some(sue4),
      List(sue4, sue3, sue2, sue1),
      Map(2014 -> TreeSet(sue4, sue3, sue2, sue1))
    )

    filtered.companyMarketValues.foreach(_ should be(marketVal))
    filtered.companyBMratio.foreach(_ should be(bMratio))
    filtered.companySize.foreach(_ should be(sizeP))
    filtered.companyDailyFinData.parameterDividends should be(dividend)
    filtered.companyDailyFinData.parameterQuotes should be(quote)
    filtered.companyDailyFinData.parameterSUEs should be(sue)
  }
}
