package model.yearlyFinancialParametersTest

import model.SymYear
import org.scalatest.{FlatSpec, Matchers}
import model.yearlyFinancialParameters._

import scala.collection.immutable.TreeMap
import utils.ordered.OrderedSyntax._
import utils.ordered.OrderedSyntax._
import utils.readers.ReadableDefaults.ErrorValidation

import scalaz.Success

class CompanyYearlyFinDataSpec extends FlatSpec with Matchers {
  "readCompanyEntries() " should "read all company yearly parameters with bad entries filtered out" in {
    import utils.readers.ReadableDefaults.CompanyYearlyFinDataReader._
    val symbol = "AFCB"

    val emptyBookValue = CompanyYearlyFinParameter(
        symbol, null, null, TreeMap.empty[SymYear, CompanyYearlyFinDataEntry], List.empty[CompanyYearlyFinDataEntry]
    )
    val emptyShares = CompanyYearlyFinParameter(
        symbol, null, null, TreeMap.empty[SymYear, CompanyYearlyFinDataEntry], List.empty[CompanyYearlyFinDataEntry]
    )
    val emptyROE = CompanyYearlyFinParameter(
        symbol, null, null, TreeMap.empty[SymYear, CompanyYearlyFinDataEntry], List.empty[CompanyYearlyFinDataEntry]
    )
    val emptyAccrual = CompanyYearlyFinParameter(
        symbol, null, null, TreeMap.empty[SymYear, CompanyYearlyFinDataEntry], List.empty[CompanyYearlyFinDataEntry]
    )

      CompanyYearlyFinData("AFCB", emptyBookValue, emptyShares, emptyROE, emptyAccrual)

    val inputCompany: ErrorValidation[CompanyYearlyFinData] = readDataFromFile("AFCB")

    val inputBookValue = CompanyYearlyFinParameter(
        symbol,
        Some(CompanyYearlyFinDataEntry(symbol, 17.8500003814697, 2010)),
        Some(CompanyYearlyFinDataEntry(symbol, 23.6900005340576, 2014)),
        TreeMap(
          SymYear(symbol, 2010) -> CompanyYearlyFinDataEntry(symbol, 17.8500003814697, 2010),
            SymYear(symbol, 2011) -> CompanyYearlyFinDataEntry(symbol, 18.8199996948242, 2011),
            SymYear(symbol, 2012) -> CompanyYearlyFinDataEntry(symbol, 20.3500003814697, 2012),
          SymYear(symbol, 2013) -> CompanyYearlyFinDataEntry(symbol, 21.7399997711182, 2013),
          SymYear(symbol, 2014) -> CompanyYearlyFinDataEntry(symbol, 23.6900005340576, 2014)
        ),

        List(
          CompanyYearlyFinDataEntry(symbol, 17.8500003814697, 2010),
            CompanyYearlyFinDataEntry(symbol, 18.8199996948242, 2011),
          CompanyYearlyFinDataEntry(symbol, 20.3500003814697, 2012),
          CompanyYearlyFinDataEntry(symbol, 21.7399997711182, 2013),
          CompanyYearlyFinDataEntry(symbol, 23.6900005340576, 2014)
        )
    )
    val inputShares = CompanyYearlyFinParameter(
        symbol,
        Some(CompanyYearlyFinDataEntry(symbol, 3, 2010)),
        Some(CompanyYearlyFinDataEntry(symbol, 2, 2014)),
        TreeMap(
          SymYear(symbol, 2010) -> CompanyYearlyFinDataEntry(symbol, 3, 2010),
          SymYear(symbol, 2011) -> CompanyYearlyFinDataEntry(symbol, 3, 2011),
          SymYear(symbol, 2012) -> CompanyYearlyFinDataEntry(symbol, 2, 2012),
          SymYear(symbol, 2013) -> CompanyYearlyFinDataEntry(symbol, 2, 2013),
          SymYear(symbol, 2014) -> CompanyYearlyFinDataEntry(symbol, 2, 2014)
        ),

        List(
          CompanyYearlyFinDataEntry(symbol, 3, 2010),
          CompanyYearlyFinDataEntry(symbol, 3, 2011),
          CompanyYearlyFinDataEntry(symbol, 2, 2012),
          CompanyYearlyFinDataEntry(symbol, 2, 2013),
          CompanyYearlyFinDataEntry(symbol, 2, 2014)
        )
    )
    val inputROE = CompanyYearlyFinParameter(
        symbol,
        Some(CompanyYearlyFinDataEntry(symbol, 0.340000003576279, 2010)),
        Some(CompanyYearlyFinDataEntry(symbol, 1.51999998092651, 2014)),
        TreeMap(
          SymYear(symbol, 2010) -> CompanyYearlyFinDataEntry(symbol, 0.340000003576279, 2010),
          SymYear(symbol, 2011) -> CompanyYearlyFinDataEntry(symbol, 0.75, 2011),
          SymYear(symbol, 2012) -> CompanyYearlyFinDataEntry(symbol, 1.0900000333786, 2012),
          SymYear(symbol, 2013) -> CompanyYearlyFinDataEntry(symbol, 1.12999999523163, 2013),
          SymYear(symbol, 2014) -> CompanyYearlyFinDataEntry(symbol, 1.51999998092651, 2014)
        ),

        List(
          CompanyYearlyFinDataEntry(symbol, 0.340000003576279, 2010),
          CompanyYearlyFinDataEntry(symbol, 0.75, 2011),
          CompanyYearlyFinDataEntry(symbol, 1.0900000333786, 2012),
          CompanyYearlyFinDataEntry(symbol, 1.12999999523163, 2013),
          CompanyYearlyFinDataEntry(symbol, 1.51999998092651, 2014)
        )
    )
    val inputAccrual = CompanyYearlyFinParameter(
        symbol,
        Some(CompanyYearlyFinDataEntry(symbol, 1, 2010)),
        Some(CompanyYearlyFinDataEntry(symbol, 3, 2014)),
        TreeMap(
          SymYear(symbol, 2010) -> CompanyYearlyFinDataEntry(symbol, 1, 2010),
          SymYear(symbol, 2011) -> CompanyYearlyFinDataEntry(symbol, 2, 2011),
          SymYear(symbol, 2012) -> CompanyYearlyFinDataEntry(symbol, 3, 2012),
          SymYear(symbol, 2013) -> CompanyYearlyFinDataEntry(symbol, 2, 2013),
          SymYear(symbol, 2014) -> CompanyYearlyFinDataEntry(symbol, 3, 2014)
        ),

        List(
          CompanyYearlyFinDataEntry(symbol, 1, 2010),
          CompanyYearlyFinDataEntry(symbol, 2, 2011),
          CompanyYearlyFinDataEntry(symbol, 3, 2012),
          CompanyYearlyFinDataEntry(symbol, 2, 2013),
          CompanyYearlyFinDataEntry(symbol, 3, 2014)
        )
    )
    inputCompany should be(Success(CompanyYearlyFinData("AFCB", inputBookValue, inputShares, inputROE, inputAccrual)))
  }
}
