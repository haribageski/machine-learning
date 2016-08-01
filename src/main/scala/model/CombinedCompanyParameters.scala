package model

import model.sentiment.CompanyNewsSentiment
import model.yearlyFinancialParameters.CompanyYearlyExtendedFinData

case class CombinedCompanyParameters(symbol: String, yearlyExtendedFinData: CompanyYearlyExtendedFinData,
                                     newsSentiment: CompanyNewsSentiment)


