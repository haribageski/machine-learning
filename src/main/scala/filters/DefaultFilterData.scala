package filters

import model.CombinedCompanyParameters
import model.dailyFinancialParameters.{CompanyDailyFinData, CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import model.yearlyFinancialParameters.{CompanyExtendedFinData, CompanyYearlyFinData, CompanyYearlyFinParameter}
import FilterSyntax._
import filters.DefaultFilterParameterGivenYears._
import filters.DefaultFilterParameterGivenDates._
import org.joda.time.DateTime

object DefaultFilterData {

  implicit object CompanyYearlyFinDataFilter extends FilterData[CompanyYearlyFinData] {
    override def applyFilter(value: CompanyYearlyFinData): CompanyYearlyFinData = {
      val consistentYears = value.shares.allCompanyEntriesOfOneYearlyParam.map(_.year)
        .intersect(value.rOE.allCompanyEntriesOfOneYearlyParam.map(_.year))
        .intersect(value.bookValue.allCompanyEntriesOfOneYearlyParam.map(_.year))
        .intersect(value.accrual.allCompanyEntriesOfOneYearlyParam.map(_.year))
        .toSet

      value.copy(
        bookValue = value.bookValue.filter(consistentYears),
        shares = value.shares.filter(consistentYears),
        rOE = value.rOE.filter(consistentYears),
        accrual = value.accrual.filter(consistentYears)
      )
    }
  }

  implicit object CompanyDailyFinDataFilter extends FilterData[CompanyDailyFinData] {
    override def applyFilter(companyDailyFinData: CompanyDailyFinData): CompanyDailyFinData = {
      val symbol = companyDailyFinData.symbol
      val parameterDividends = companyDailyFinData.parameterDividends.allCompanyEntriesOfOneDailyParam.map(_.date).toSet
      val parameterQuotes = companyDailyFinData.parameterQuotes.allCompanyEntriesOfOneDailyParam.map(_.date).toSet
      val parameterSUEs = companyDailyFinData.parameterSUEs.allCompanyEntriesOfOneDailyParam.map(_.date).toSet

      val intersectAllThreeDailyParamsYears =
        parameterDividends
          .intersect(parameterSUEs)
          .intersect(parameterQuotes)
        .filter(dateTime => parameterQuotes.contains(dateTime.plusDays(1)))

      val withConsecutiveQuotes = parameterQuotes.intersect(intersectAllThreeDailyParamsYears) ++
        parameterQuotes.intersect(intersectAllThreeDailyParamsYears.map(_.plusDays(1)))

      val dividendsToAdd = companyDailyFinData.parameterDividends.allCompanyEntriesOfOneDailyParam.filter(param =>
        intersectAllThreeDailyParamsYears.contains(param.date))
      val quotesToAdd = companyDailyFinData.parameterQuotes.allCompanyEntriesOfOneDailyParam.filter(param =>
        withConsecutiveQuotes.contains(param.date))
      val sUEsToAdd = companyDailyFinData.parameterSUEs.allCompanyEntriesOfOneDailyParam.filter(param =>
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
      val companyDailyFinDataFiltered = companyExtendedFinData.companyDailyFinData.filter   //returns consistent dailyFinData
      val companyExtendedWithDerivedParams: CompanyExtendedFinData =
        companyExtendedFinData.copy(companyDailyFinData = companyDailyFinDataFiltered).deriveAdditionalFinParameters
      val companyYearlyFinData = companyExtendedWithDerivedParams.companyYearlyFinData.filter
      val companyBMratio = companyExtendedWithDerivedParams.companyBMratio
      val companyMarketValues = companyExtendedWithDerivedParams.companyMarketValues
      val companySize = companyExtendedWithDerivedParams.companySize

      val sym = companyYearlyFinData.symbol
      val consistentYears: Set[Int] = companyYearlyFinData.accrual.allCompanyEntriesOfOneYearlyParam.map(_.year).toSet
        .intersect(companyBMratio.map(_.perYearM.keySet.map(_.year).toSet).getOrElse(Set.empty[Int]))
        .intersect(companyMarketValues.map(_.perYearM.keySet.map(_.year).toSet).getOrElse(Set.empty[Int]))
        .intersect(companySize.map(_.perYearM.keySet.map(_.year).toSet).getOrElse(Set.empty[Int]))

      val consistentDatesGivenYears: Set[DateTime] =
        companyDailyFinDataFiltered.parameterDividends.allCompanyEntriesOfOneDailyParam.map(_.date).toSet
        .filter(date => consistentYears.contains(date.getYear))
      val consistentYearsGivenDates = consistentYears.intersect(consistentDatesGivenYears.map(_.getYear))

      val paramAccrual: CompanyYearlyFinParameter = companyYearlyFinData.accrual.filter(consistentYearsGivenDates)

      val paramBookVal = companyYearlyFinData.bookValue.filter(consistentYearsGivenDates)

      val paramROE = companyYearlyFinData.rOE.filter(consistentYearsGivenDates)

      val paramShares = companyYearlyFinData.shares.filter(consistentYearsGivenDates)

      val paramMarketVal: Option[CompanyYearlyFinParameter] = companyMarketValues.map(_.filter(consistentYearsGivenDates))

      val paramSize: Option[CompanyYearlyFinParameter] = companySize.map(_.filter(consistentYearsGivenDates))

      val paramDividend = companyDailyFinDataFiltered.parameterDividends.filter(consistentDatesGivenYears)

      val paramQuotes = companyDailyFinDataFiltered.parameterQuotes
        .filter(consistentDatesGivenYears ++ consistentDatesGivenYears.map(_.plusDays(1)))

      val paramSUE = companyDailyFinDataFiltered.parameterSUEs.filter(consistentDatesGivenYears)

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


      val consistentDatesOfFinDataIntersectSentimentDates: Set[DateTime] =
        consistentFinancialData.companyDailyFinData.parameterDividends.allCompanyEntriesOfOneDailyParam.map(_.date).toSet
        .intersect(allParameters.newsSentiment.map(_.avgSentiPerDateDescript.keySet).getOrElse(Set.empty[DateTime]))

      val consistentSentimentData = allParameters.newsSentiment.map(_.filter(consistentDatesOfFinDataIntersectSentimentDates))

      val consistentDailyFinData =
        consistentFinancialData.companyDailyFinData.filter(consistentDatesOfFinDataIntersectSentimentDates)

      val consistentYearlyFinData =
        consistentFinancialData.companyYearlyFinData.filter(consistentDatesOfFinDataIntersectSentimentDates.map(_.getYear))


      CombinedCompanyParameters(
        allParameters.symbol,
        CompanyExtendedFinData(
          consistentYearlyFinData,
          consistentDailyFinData,
          None,
          None,
          None
        ).deriveAdditionalFinParameters,
        consistentSentimentData
      )
    }
  }
}
