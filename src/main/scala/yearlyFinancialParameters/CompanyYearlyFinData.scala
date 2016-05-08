package yearlyFinancialParameters

import utils.DateExtended

case class CompanyYearlyFinData (
                             symbol: String,
                             bookValue: CompanyYearlyFinParameter,
                             shares: CompanyYearlyFinParameter,
                             rOE: CompanyYearlyFinParameter,
                             accrual: CompanyYearlyFinParameter
                           ) {
  override def toString =
  s"symbol: $symbol;\n bookValue: $bookValue;\n shares: $shares;\n ROE: $rOE;\n accrual: $accrual"
}


object CompanyYearlyFinData {
  def apply(sym: String): CompanyYearlyFinData = {
    apply(
    sym, CompanyYearlyFinParameter(sym), CompanyYearlyFinParameter(sym), CompanyYearlyFinParameter(sym),
      CompanyYearlyFinParameter(sym)
    )
  }
}
