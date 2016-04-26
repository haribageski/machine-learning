//package yearlyFinancialParametersTest
//
//import org.scalatest.{Matchers, FlatSpec}
//import utils.SymYear
//import yearlyFinancialParameters.{CompanyYearlyFinData, ParameterShares, CompanyYearlyFinParameter, YearlyFinDataEntry}
//
//class ParameterSharesTest extends FlatSpec with Matchers {
//
//  "ParameterDividends.dailyparameter.synchronizeEntries()" should
//    "return parameter that contains only consistent in year entries" in {
//
//    val sym = "EZPW"
//    val oldest = YearlyFinDataEntry(sym, 43, 2007)
//    val youngest = YearlyFinDataEntry(sym, 51, 2012)
//
//    val shares = List(
//      YearlyFinDataEntry(sym, 43, 2007),
//      YearlyFinDataEntry(sym, 43, 2008),
//      YearlyFinDataEntry(sym, 51, 2012)
//    )
//    val consistentYears = Set(2007, 2008, 2012)
//
//    val mapConsistent = Map(
//      SymYear(sym, 2007) -> YearlyFinDataEntry(sym, 43, 2007),
//      SymYear(sym, 2008) -> YearlyFinDataEntry(sym, 43, 2008),
//      SymYear(sym, 2012) -> YearlyFinDataEntry(sym, 51, 2012)
//    )
//
//    val sharesRead: ParameterShares = CompanyYearlyFinData(sym).readFromFiles().shares
//    sharesRead.companyYearlyFinParameter.synchronizeEntries(consistentYears = consistentYears) should be (
//      CompanyYearlyFinParameter(sym, Some(oldest), Some(youngest), mapConsistent, shares)
//    )
//  }
//}
