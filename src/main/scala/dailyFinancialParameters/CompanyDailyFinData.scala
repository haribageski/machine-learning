package dailyFinancialParameters



case class CompanyDailyFinData(symbol: String,
                               parameterDividends: CompanyDailyFinParameter,
                               parameterQuotes: CompanyDailyFinParameter,
                               parameterSUEs: CompanyDailyFinParameter)



//  override def toString =
//    s"dividends: $parameterDividends;\n quotes: $parameterQuotes;\n SUE: $parameterSUEs"


object CompanyDailyFinData {
  def apply(sym: String): CompanyDailyFinData = {
    apply(
      sym: String,
      CompanyDailyFinParameter(sym),
      CompanyDailyFinParameter(sym),
      CompanyDailyFinParameter(sym)
    )
  }
}
