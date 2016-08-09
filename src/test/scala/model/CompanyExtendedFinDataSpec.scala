package model

import filters.DefaultFilterData._
import filters.FilterSyntax
import filters.FilterSyntax.FilterOps
import model.dailyFinancialParameters._
import model.yearlyFinancialParameters.{CompanyExtendedFinData, CompanyYearlyFinDataEntry, CompanyYearlyFinParameter}
import org.scalatest.{FlatSpec, Matchers}
import utils.ordered.OrderedSyntax._
import utils.readers.ReadableDefaults.CompanyDailyFinDataReader
import utils.readers.ReadableDefaults.CompanyYearlyFinDataReader.readDataFromFile

import scala.collection.immutable.{TreeMap, TreeSet}

class CompanyExtendedFinDataSpec  extends FlatSpec with Matchers {

  val sym = "Example"
  val companyYearlyExtendedFinData = CompanyExtendedFinData(
    readDataFromFile(sym),
    CompanyDailyFinDataReader.readDataFromFile(sym)
      .filter
  )

  val companyExtendedWithDerivedParams: CompanyExtendedFinData =
    companyYearlyExtendedFinData.deriveAdditionalFinParameters()
  println("companySize:" + companyExtendedWithDerivedParams.companySize)

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

  val companyExtendedFinDataWithAllParams = CompanyExtendedFinData(
    readDataFromFile(sym),
    CompanyDailyFinDataReader.readDataFromFile(sym)
      .filter,
    Some(marketVal),
    Some(bMratio),
    Some(sizeP)
  )

  "deriveAdditionalFinParameters()" should "derive MarketVal, BMratio, and Size" in {
    //three derived parameters
    companyExtendedWithDerivedParams.companyBMratio.foreach(_ should be(bMratio))
    companyExtendedWithDerivedParams.companyMarketValues.foreach(_ should be(marketVal))
    companyExtendedWithDerivedParams.companySize.foreach(_ should be(sizeP))
  }


  "filterInconsistentEntries()" should "filter out inconsistent entries" in {
    val filtered: CompanyExtendedFinData = companyExtendedWithDerivedParams

    val divi1 = CompanyDailyFinDataEntry(sym, 0.01, DateExtended.fromString("04/01/2014"))
    val divi2 = CompanyDailyFinDataEntry(sym, 0.02, DateExtended.fromString("04/04/2014"))
    val dividend = CompanyDailyFinParameter(
      sym,
      Some(divi1),
      Some(divi2),
      List(divi2, divi1),
      Map(2014 -> TreeSet(divi1, divi2))
    )

    val quote1 = CompanyDailyFinDataEntry(sym, 18, DateExtended.fromString("04/01/2014"))
    val quote2 = CompanyDailyFinDataEntry(sym, 22, DateExtended.fromString("04/04/2014"))
    val quote3 = CompanyDailyFinDataEntry(sym, 18, DateExtended.fromString("05/01/2014"))
    val quote4 = CompanyDailyFinDataEntry(sym, 22, DateExtended.fromString("05/04/2014"))

    val quote = CompanyDailyFinParameter(
      sym,
      Some(quote1),
      Some(quote4),
      List(quote4, quote2, quote3, quote1),
      Map(2014 -> TreeSet(quote1, quote3, quote2, quote4))
    )

    val sue1 = CompanyDailyFinDataEntry(sym, 1.51999998092651, DateExtended.fromString("04/01/2014"))
    val sue2 = CompanyDailyFinDataEntry(sym, 1.51999998092651, DateExtended.fromString("04/04/2014"))
    val sue = CompanyDailyFinParameter(
      sym,
      Some(sue1),
      Some(sue2),
      List(sue2, sue1),
      Map(2014 -> TreeSet(sue2, sue1))
    )

    filtered.companyMarketValues.foreach(_ should be(marketVal))
    filtered.companyBMratio.foreach(_ should be(bMratio))
    filtered.companySize.foreach(_ should be(sizeP))
    filtered.companyDailyFinData.parameterDividends should be(dividend)
    filtered.companyDailyFinData.parameterQuotes should be(quote)
    filtered.companyDailyFinData.parameterSUEs should be(sue)
  }
}
