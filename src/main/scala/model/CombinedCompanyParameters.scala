package model

import analyzers.SentimentAnalyzer
import model.sentiment.CompanyNewsSentiment
import model.yearlyFinancialParameters.CompanyYearlyExtendedFinData
import utils.readers.ReadableDefaults.CompanyDailyFinDataReader
import utils.readers.ReadableDefaults.CompanyYearlyFinDataReader.readDataFromFile

case class CombinedCompanyParameters(symbol: String, yearlyExtendedFinData: CompanyYearlyExtendedFinData,
                                     newsSentiment: CompanyNewsSentiment)


object CombinedCompanyParameters {
  def apply(sym: String): CombinedCompanyParameters = {
    apply(
      sym,
      CompanyYearlyExtendedFinData(
        readDataFromFile(sym),
        CompanyDailyFinDataReader.readDataFromFile(sym)
      ),
      SentimentAnalyzer.evaluateSentiOfAllCompanyNews(sym)
    )
  }
}

