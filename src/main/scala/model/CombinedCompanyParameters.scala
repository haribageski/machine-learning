package model

import analyzers.SentimentAnalyzer
import model.sentiment.CompanyNewsSentiment
import model.yearlyFinancialParameters.CompanyYearlyExtendedFinData
import utils.readers.ReadableDefaults.{CompanyDailyFinDataReader, CompanyNewsReader}
import utils.readers.ReadableDefaults.CompanyYearlyFinDataReader.readDataFromFile

case class CombinedCompanyParameters(symbol: String, yearlyExtendedFinData: CompanyYearlyExtendedFinData,
                                     newsSentiment: CompanyNewsSentiment)


object CombinedCompanyParameters {
  def apply(sym: String): CombinedCompanyParameters = {
    val allCompanyNews = CompanyNewsReader.readDataFromFile(sym)

    apply(
      sym,
      CompanyYearlyExtendedFinData(
        readDataFromFile(sym),
        CompanyDailyFinDataReader.readDataFromFile(sym)
      ),
      SentimentAnalyzer.evaluateSentiOfAllCompanyNews(allCompanyNews)
    )
  }
}

