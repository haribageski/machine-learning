package filters

import model.CombinedCompanyParameters
import model.dailyFinancialParameters.{CompanyDailyFinData, CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import model.yearlyFinancialParameters.{CompanyExtendedFinData, CompanyYearlyFinData, CompanyYearlyFinParameter}
import FilterSyntax._
import filters.DefaultFilterParameterGivenYears._
import filters.DefaultFilterParameterGivenDates._
import org.joda.time.DateTime

object DefaultFilterData {

  implicit object CompanyDailyFinDataFilter extends FilterData[CompanyDailyFinData] {
    override def applyFilter(companyDailyFinData: CompanyDailyFinData): CompanyDailyFinData = {
      val symbol = companyDailyFinData.symbol
      val parameterDividends = companyDailyFinData.parameterDividends
      val parameterQuotes = companyDailyFinData.parameterQuotes
      val parameterSUEs = companyDailyFinData.parameterSUEs

      val allQuotesDates: List[DateTime] = parameterQuotes.allCompanyEntriesOfOneDailyParam.map(_.date)
      val relevantQuotesDates = allQuotesDates.filter(date => allQuotesDates.contains(date.plusDays(1)))

      val intersectAllThreeDailyParamsYears =
        relevantQuotesDates
          .intersect(parameterDividends.allCompanyEntriesOfOneDailyParam.map(_.date))
          .intersect(parameterSUEs.allCompanyEntriesOfOneDailyParam.map(_.date))

      val dividendsToAdd = parameterDividends.allCompanyEntriesOfOneDailyParam.filter(param =>
        intersectAllThreeDailyParamsYears.contains(param.date))
      val quotesToAdd = parameterQuotes.allCompanyEntriesOfOneDailyParam.filter(param =>
        intersectAllThreeDailyParamsYears.contains(param.date))
      val sUEsToAdd = parameterSUEs.allCompanyEntriesOfOneDailyParam.filter(param =>
        intersectAllThreeDailyParamsYears.contains(param.date))


      val synchronizedParameterDividends = CompanyDailyFinParameter(symbol).addEntries(dividendsToAdd)
      val synchronizedParameterQuotes = CompanyDailyFinParameter(symbol).addEntries(quotesToAdd)
      val synchronizedparameterSUEs = CompanyDailyFinParameter(symbol).addEntries(sUEsToAdd)

      CompanyDailyFinData(symbol, synchronizedParameterDividends, synchronizedParameterQuotes, synchronizedparameterSUEs)
    }
  }



  implicit object CompanyExtendedFinDataFilter
    extends FilterData[CompanyExtendedFinData] {

    /**
      * Deletes the inconsistent entries that have data for some but not all parameters. It does that by
      * tail-recursively iterating through all the entries of each parameter, and tail-recursively constructing new
      * parameters. NaN values are already filtered out.
      */
    override def applyFilter(companyExtendedFinData: CompanyExtendedFinData): CompanyExtendedFinData = {
      val companyYearlyFinData = companyExtendedFinData.companyYearlyFinData
      val companyBMratio = companyExtendedFinData.companyBMratio
      val companyMarketValues = companyExtendedFinData.companyMarketValues
      val companySize = companyExtendedFinData.companySize
      val companyDailyFinData = companyExtendedFinData.companyDailyFinData.filter   //returns consistent dailyFinData

      //persistent entries in the extended parameters are those in BMratio
      val sym = companyYearlyFinData.symbol
      val consistentYears: Set[Int] = companyBMratio.map(_.perYearM.keySet.map(_.year).toSet).getOrElse(Set.empty[Int])

      val paramAccrual: CompanyYearlyFinParameter = companyYearlyFinData.accrual.filter(consistentYears)

      val paramBookVal = companyYearlyFinData.bookValue.filter(consistentYears)

      val paramROE = companyYearlyFinData.rOE.filter(consistentYears)

      val paramShares = companyYearlyFinData.shares.filter(consistentYears)

      val paramMarketVal: Option[CompanyYearlyFinParameter] = companyMarketValues.map(_.filter(consistentYears))

      val paramSize: Option[CompanyYearlyFinParameter] = companySize.map(_.filter(consistentYears))

      val paramDividend = companyDailyFinData.parameterDividends.filter(consistentYears)

      val paramQuotes = companyDailyFinData.parameterQuotes.filter(consistentYears)

      val paramSUE = companyDailyFinData.parameterSUEs.filter(consistentYears)

      CompanyExtendedFinData(
        CompanyYearlyFinData(
          sym,
          paramBookVal,
          paramShares,
          paramROE,
          paramAccrual
        ),
        CompanyDailyFinData(
          sym,
          paramDividend,
          paramQuotes,
          paramSUE
        ),
        paramMarketVal,
        companyBMratio,
        paramSize
      )
    }
  }





  /**
    * Produces the final combination of parameters for which there is at least one daily fin param for each year
    * for which there is a yearly param; at least one sentimentValue in same date with one Quote,
    * and in year for which there is daliy fin param.
    */
  implicit object CombinedCompanyParametersFilter extends FilterData[CombinedCompanyParameters] {
    override def applyFilter(allParameters: CombinedCompanyParameters): CombinedCompanyParameters = {

      val consistentFinancialData: CompanyExtendedFinData =
        allParameters.extendedFinData.filter

      val dividendAllEntries: List[CompanyDailyFinDataEntry] =
        consistentFinancialData.companyDailyFinData.parameterDividends.allCompanyEntriesOfOneDailyParam

      val consistentSentimentData = allParameters.newsSentiment.filter {
        dividendAllEntries.map(_.date).toSet
      }

      val consistentDividendsAllEntries: List[CompanyDailyFinDataEntry] =
        dividendAllEntries.filter { finData =>
          consistentSentimentData.avgSentiPerDateDescript.keySet.contains(finData.date)
        }

      val consistentDailyFinData =
        consistentFinancialData.companyDailyFinData.filter(consistentDividendsAllEntries.map(_.date).toSet)

      val consistentYearlyFinData =
        consistentFinancialData.companyYearlyFinData.filter(consistentDailyFinData.parameterDividends.groupedByYearM.keySet)


      CombinedCompanyParameters(
        allParameters.symbol,
        CompanyExtendedFinData(
          consistentYearlyFinData,
          consistentDailyFinData,
          allParameters.extendedFinData.companyMarketValues.map(_.filter(consistentYearlyFinData.accrual.perYearM.keySet.map(_.year))),
          allParameters.extendedFinData.companyBMratio.map(_.filter(consistentYearlyFinData.accrual.perYearM.keySet.map(_.year))),
          allParameters.extendedFinData.companySize.map(_.filter(consistentYearlyFinData.accrual.perYearM.keySet.map(_.year)))
        ),
        consistentSentimentData
      )
    }
  }
}
