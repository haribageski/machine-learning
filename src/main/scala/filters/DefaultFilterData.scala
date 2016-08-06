package filters

import model.CombinedCompanyParameters
import model.dailyFinancialParameters.{CompanyDailyFinData, CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import model.yearlyFinancialParameters.{CompanyYearlyExtendedFinData, CompanyYearlyFinData, CompanyYearlyFinParameter}
import FilterSyntax._
import filters.DefaultFilterParameterGivenYears._
import filters.DefaultFilterParameterGivenDates._

object DefaultFilterData {

  implicit object CompanyDailyFinDataFilter extends FilterData[CompanyDailyFinData] {
    override def applyFilter(companyDailyFinData: CompanyDailyFinData): CompanyDailyFinData = {
      val symbol = companyDailyFinData.symbol
      val parameterDividends = companyDailyFinData.parameterDividends
      val parameterQuotes = companyDailyFinData.parameterQuotes
      val parameterSUEs = companyDailyFinData.parameterSUEs

      val intersectAllThreeDailyParamsYears: List[Int] =
        parameterDividends.allCompanyEntriesOfOneDailyParam.map(_.date.getYear)
          .intersect(parameterQuotes.allCompanyEntriesOfOneDailyParam.map(_.date.getYear))
          .intersect(parameterSUEs.allCompanyEntriesOfOneDailyParam.map(_.date.getYear))

      val dividendsToAdd = parameterDividends.allCompanyEntriesOfOneDailyParam.filter(param =>
        intersectAllThreeDailyParamsYears.contains(param.date.getYear))
      val quotesToAdd = parameterQuotes.allCompanyEntriesOfOneDailyParam.filter(param =>
        intersectAllThreeDailyParamsYears.contains(param.date.getYear))
      val sUEsToAdd = parameterSUEs.allCompanyEntriesOfOneDailyParam.filter(param =>
        intersectAllThreeDailyParamsYears.contains(param.date.getYear))


      val synchronizedParameterDividends = CompanyDailyFinParameter(symbol).addEntries(dividendsToAdd)
      val synchronizedParameterQuotes = CompanyDailyFinParameter(symbol).addEntries(quotesToAdd)
      val synchronizedparameterSUEs = CompanyDailyFinParameter(symbol).addEntries(sUEsToAdd)

      CompanyDailyFinData(symbol, synchronizedParameterDividends, synchronizedParameterQuotes, synchronizedparameterSUEs)
    }
  }



  implicit object CompanyYearlyExtendedFinDataFilter
    extends FilterData[CompanyYearlyExtendedFinData] {

    /**
      * Deletes the inconsistent entries that have data for some but not all parameters. It does that by
      * tail-recursively iterating through all the entries of each parameter, and tail-recursively constructing new
      * parameters. NaN values are already filtered out.
      */
    override def applyFilter(companyYearlyExtendedFinData: CompanyYearlyExtendedFinData): CompanyYearlyExtendedFinData = {
      val companyYearlyFinData = companyYearlyExtendedFinData.companyYearlyFinData
      val companyBMratio = companyYearlyExtendedFinData.companyBMratio
      val companyMarketValues = companyYearlyExtendedFinData.companyMarketValues
      val companySize = companyYearlyExtendedFinData.companySize
      val companyDailyFinData = companyYearlyExtendedFinData.companyDailyFinData

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

      CompanyYearlyExtendedFinData(
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
    * for which there is a yearly param; at least one sentimentValue in same date with one SUE, with date preceeding
    * another SUE, and in year for which there is daliy fin param.
    */
  implicit object CombinedCompanyParametersFilter extends FilterData[CombinedCompanyParameters] {
    override def applyFilter(allParameters: CombinedCompanyParameters): CombinedCompanyParameters = {

      val consistentFinancialData: CompanyYearlyExtendedFinData =
        allParameters.yearlyExtendedFinData.filter

      val sUEsAllEntries: List[CompanyDailyFinDataEntry] =
        allParameters.yearlyExtendedFinData.companyDailyFinData.parameterSUEs.allCompanyEntriesOfOneDailyParam

      val consistentSentimentData = allParameters.newsSentiment.filter {
        sUEsAllEntries.map(_.date).toSet
      }

      val consistentSUEsAllEntries: List[CompanyDailyFinDataEntry] =
        sUEsAllEntries.filter { finData =>
          consistentSentimentData.avgSentiPerDateDescript.keySet.contains(finData.date) ||
            consistentSentimentData.avgSentiPerDateDescript.keySet.contains(finData.date.minusDays(1))
        }

      val consistentDailyFinData =
        consistentFinancialData.companyDailyFinData.filter(consistentSUEsAllEntries.map(_.date).toSet)

      val consistentYearlyFinData =
        consistentFinancialData.companyYearlyFinData.filter(consistentDailyFinData.parameterSUEs.groupedByYearM.keySet)


      CombinedCompanyParameters(
        allParameters.symbol,
        CompanyYearlyExtendedFinData(
          consistentYearlyFinData,
          consistentDailyFinData,
          allParameters.yearlyExtendedFinData.companyMarketValues.map(_.filter(consistentYearlyFinData.accrual.perYearM.keySet.map(_.year))),
          allParameters.yearlyExtendedFinData.companyBMratio.map(_.filter(consistentYearlyFinData.accrual.perYearM.keySet.map(_.year))),
          allParameters.yearlyExtendedFinData.companySize.map(_.filter(consistentYearlyFinData.accrual.perYearM.keySet.map(_.year)))
        ),
        consistentSentimentData
      )
    }
  }
}
