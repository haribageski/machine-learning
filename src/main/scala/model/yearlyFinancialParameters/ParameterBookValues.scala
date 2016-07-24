//package yearlyFinancialParameters
//
//import utils.SymYear
//
//import scala.collection.immutable.TreeMap
//
//case class ParameterBookValues(companyYearlyFinParameter: CompanyYearlyFinParameter){
//  private val indexOfValue = 2
//
//  def addBookValue(entry: YearlyFinDataEntry): ParameterBookValues = entry.value match {
//    case 0 => this //we must not have Book Values with value equals to 0
//    case _ =>
//      val updatedCompanyYearlyFinData: CompanyYearlyFinParameter = companyYearlyFinParameter.addEntry(entry)
//      ParameterBookValues(updatedCompanyYearlyFinData)
//  }
//}
//
//object ParameterBookValues {
//  def apply(symbol: String):ParameterBookValues = apply(
//    CompanyYearlyFinParameter(
//      symbol, None, None, TreeMap.empty[SymYear, YearlyFinDataEntry], List.empty[YearlyFinDataEntry]
//    )
//  )
//}
