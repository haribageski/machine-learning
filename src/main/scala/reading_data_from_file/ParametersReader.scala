//package reading_data_from_file
//
//import dailyFinancialParameters.CompanyDailyFinParameter
//
///**
//  * Provided (empty) CompanyDailyFinParameter with defined symbol, the ParametersReader uses
//  * CompanyDailyFinParameter.readCompanyEntries() to read the dividends/quotes/SUEs and return new
//  * CompanyDailyFinParameter with all the read entries.
//  */
//object ParametersReader {
//  def readCompanyDividends(dailyParameter: CompanyDailyFinParameter): CompanyDailyFinParameter = {
//    val filePath = "resources/dividends/" + dailyParameter.symbol + ".txt"
//    val indexOfValue = 2
//    val inputDailyFinData: CompanyDailyFinParameter = dailyParameter.readCompanyEntries(filePath, indexOfValue)
//    inputDailyFinData
//  }
//
//  def readCompanyQuotes(dailyParameter: CompanyDailyFinParameter): CompanyDailyFinParameter = {
//    val filePath = "resources/quotes-prices/" + dailyParameter.symbol + ".txt"
//    val indexOfValue = 3
//    val inputDailyFinData: CompanyDailyFinParameter = dailyParameter.readCompanyEntries(filePath, indexOfValue)
//    inputDailyFinData
//  }
//
//  def readCompanySUEs(dailyParameter: CompanyDailyFinParameter): CompanyDailyFinParameter = {
//    val filePath = "resources/earning-surprises/" + dailyParameter.symbol + ".txt"
//    val indexOfValue = 2
//    val inputDailyFinData: CompanyDailyFinParameter = dailyParameter.readCompanyEntries(filePath, indexOfValue)
//    inputDailyFinData
//  }
//}
