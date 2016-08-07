package model

import model.sentiment.CompanyNewsSentiment
import model.yearlyFinancialParameters.CompanyExtendedFinData

case class CombinedCompanyParameters(symbol: String, extendedFinData: CompanyExtendedFinData,
                                     newsSentiment: CompanyNewsSentiment)


