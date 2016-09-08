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
        symbol, null, null, TreeMap.empty[Int, CompanyYearlyFinDataEntry], List.empty[CompanyYearlyFinDataEntry]
    )
    val emptyShares = CompanyYearlyFinParameter(
        symbol, null, null, TreeMap.empty[Int, CompanyYearlyFinDataEntry], List.empty[CompanyYearlyFinDataEntry]
    )
    val emptyROE = CompanyYearlyFinParameter(
        symbol, null, null, TreeMap.empty[Int, CompanyYearlyFinDataEntry], List.empty[CompanyYearlyFinDataEntry]
    )
    val emptyAccrual = CompanyYearlyFinParameter(
        symbol, null, null, TreeMap.empty[Int, CompanyYearlyFinDataEntry], List.empty[CompanyYearlyFinDataEntry]
    )

      CompanyYearlyFinData("AFCB", emptyBookValue, emptyShares, emptyROE, emptyAccrual)

    val inputCompany: ErrorValidation[CompanyYearlyFinData] = readDataFromFile("AFCB")

    val inputBookValue = CompanyYearlyFinParameter(
        symbol,
        Some(CompanyYearlyFinDataEntry(17.8500003814697, 2010)),
        Some(CompanyYearlyFinDataEntry(23.6900005340576, 2014)),
        TreeMap(
           2010 -> CompanyYearlyFinDataEntry(17.8500003814697, 2010),
          2011 -> CompanyYearlyFinDataEntry(18.8199996948242, 2011),
            2012 -> CompanyYearlyFinDataEntry(20.3500003814697, 2012),
          2013 -> CompanyYearlyFinDataEntry(21.7399997711182, 2013),
          2014 -> CompanyYearlyFinDataEntry(23.6900005340576, 2014)
        ),

        List(
          CompanyYearlyFinDataEntry(17.8500003814697, 2010),
            CompanyYearlyFinDataEntry(18.8199996948242, 2011),
          CompanyYearlyFinDataEntry(20.3500003814697, 2012),
          CompanyYearlyFinDataEntry(21.7399997711182, 2013),
          CompanyYearlyFinDataEntry(23.6900005340576, 2014)
        )
    )
    val inputShares = CompanyYearlyFinParameter(
        symbol,
        Some(CompanyYearlyFinDataEntry(3, 2010)),
        Some(CompanyYearlyFinDataEntry(2, 2014)),
        TreeMap(
          2010 -> CompanyYearlyFinDataEntry(3, 2010),
          2011 -> CompanyYearlyFinDataEntry(3, 2011),
          2012 -> CompanyYearlyFinDataEntry(2, 2012),
          2013 -> CompanyYearlyFinDataEntry(2, 2013),
          2014 -> CompanyYearlyFinDataEntry(2, 2014)
        ),

        List(
          CompanyYearlyFinDataEntry(3, 2010),
          CompanyYearlyFinDataEntry(3, 2011),
          CompanyYearlyFinDataEntry(2, 2012),
          CompanyYearlyFinDataEntry(2, 2013),
          CompanyYearlyFinDataEntry(2, 2014)
        )
    )
    val inputROE = CompanyYearlyFinParameter(
        symbol,
        Some(CompanyYearlyFinDataEntry(0.340000003576279, 2010)),
        Some(CompanyYearlyFinDataEntry(1.51999998092651, 2014)),
        TreeMap(
          2010 -> CompanyYearlyFinDataEntry(0.340000003576279, 2010),
          2011 -> CompanyYearlyFinDataEntry(0.75, 2011),
          2012 -> CompanyYearlyFinDataEntry(1.0900000333786, 2012),
          2013 -> CompanyYearlyFinDataEntry(1.12999999523163, 2013),
          2014 -> CompanyYearlyFinDataEntry(1.51999998092651, 2014)
        ),

        List(
          CompanyYearlyFinDataEntry(0.340000003576279, 2010),
          CompanyYearlyFinDataEntry(0.75, 2011),
          CompanyYearlyFinDataEntry(1.0900000333786, 2012),
          CompanyYearlyFinDataEntry(1.12999999523163, 2013),
          CompanyYearlyFinDataEntry(1.51999998092651, 2014)
        )
    )
    val inputAccrual = CompanyYearlyFinParameter(
        symbol,
        Some(CompanyYearlyFinDataEntry(1, 2010)),
        Some(CompanyYearlyFinDataEntry(3, 2014)),
        TreeMap(
          2010 -> CompanyYearlyFinDataEntry( 1, 2010),
          2011 -> CompanyYearlyFinDataEntry( 2, 2011),
          2012 -> CompanyYearlyFinDataEntry( 3, 2012),
          2013 -> CompanyYearlyFinDataEntry( 2, 2013),
          2014 -> CompanyYearlyFinDataEntry( 3, 2014)
        ),

        List(
          CompanyYearlyFinDataEntry( 1, 2010),
          CompanyYearlyFinDataEntry( 2, 2011),
          CompanyYearlyFinDataEntry( 3, 2012),
          CompanyYearlyFinDataEntry( 2, 2013),
          CompanyYearlyFinDataEntry( 3, 2014)
        )
    )
    inputCompany should be(Success(CompanyYearlyFinData("AFCB", inputBookValue, inputShares, inputROE, inputAccrual)))
  }
}
