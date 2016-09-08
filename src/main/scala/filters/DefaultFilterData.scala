package filters

import model.CombinedCompanyParameters
import model.dailyFinancialParameters.{CompanyDailyFinData, CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import model.yearlyFinancialParameters.{CompanyExtendedFinData, CompanyYearlyFinData, CompanyYearlyFinParameter}
import FilterSyntax._
import filters.DefaultFilterParameterGivenYears._
import filters.DefaultFilterParameterGivenDates._
import model.sentiment.CompanyNewsSentiment
import org.joda.time.DateTime

import scalaz._
import scalaz.Scalaz._

object DefaultFilterData {

  implicit object CompanyYearlyFinDataFilter extends FilterData[CompanyYearlyFinData] {
    override def applyFilter(value: CompanyYearlyFinData): Option[CompanyYearlyFinData] = {
      val consistentYears = value.shares.allCompanyEntriesOfOneYearlyParam.map(_.year)
        .intersect(value.rOE.allCompanyEntriesOfOneYearlyParam.map(_.year))
        .intersect(value.bookValue.allCompanyEntriesOfOneYearlyParam.map(_.year))
        .intersect(value.accrual.allCompanyEntriesOfOneYearlyParam.map(_.year))
        .toSet

      Some(value.copy(
        bookValue = value.bookValue.filter(consistentYears),
        shares = value.shares.filter(consistentYears),
        rOE = value.rOE.filter(consistentYears),
        accrual = value.accrual.filter(consistentYears)
      ))
    }
  }

  implicit object CompanyDailyFinDataFilter extends FilterData[CompanyDailyFinData] {
    override def applyFilter(companyDailyFinData: CompanyDailyFinData): Option[CompanyDailyFinData] = {
      val symbol = companyDailyFinData.symbol
      val parameterDividendsPerYear: Map[Int, List[Double]] = companyDailyFinData.parameterDividends.groupedByYearM
      val parameterQuotes: Set[DateTime] = companyDailyFinData.parameterQuotes.allCompanyEntriesOfOneDailyParam.map(_.date).toSet
      val parameterQuotesPerYear = companyDailyFinData.parameterQuotes.groupedByYearM
      val parameterSUEsPerYear = companyDailyFinData.parameterSUEs.groupedByYearM

      val intersectAllThreeDailyParamsYears =
        parameterSUEsPerYear.keySet intersect
          parameterDividendsPerYear.keySet intersect
          parameterQuotesPerYear.keySet intersect
          parameterSUEsPerYear.keySet

      val intersectionConsecutiveQuotesExisting: Set[DateTime] =
        parameterQuotes.filter(date =>
          intersectAllThreeDailyParamsYears.contains(date.getYear) && (parameterQuotes.contains(date.plusDays(1)))
        )
      val consistentYears: Set[Int] =
        intersectAllThreeDailyParamsYears.filter(year => intersectionConsecutiveQuotesExisting.map(_.getYear) contains (year))

      val dividendsToAdd = companyDailyFinData.parameterDividends.allCompanyEntriesOfOneDailyParam.filter(param =>
        consistentYears.contains(param.date.getYear))
      val quotesToAdd = companyDailyFinData.parameterQuotes.allCompanyEntriesOfOneDailyParam.filter(param =>
        intersectionConsecutiveQuotesExisting.contains(param.date)
      )

      val sUEsToAdd = companyDailyFinData.parameterSUEs.allCompanyEntriesOfOneDailyParam.filter(param =>
        consistentYears.contains(param.date.getYear))


      val synchronizedParameterDividends = CompanyDailyFinParameter(symbol).addEntries(dividendsToAdd)
      val synchronizedParameterQuotes = CompanyDailyFinParameter(symbol).addEntries(quotesToAdd)
      val synchronizedparameterSUEs = CompanyDailyFinParameter(symbol).addEntries(sUEsToAdd)

      Some(CompanyDailyFinData(symbol, synchronizedParameterDividends, synchronizedParameterQuotes, synchronizedparameterSUEs))
    }
  }



  implicit object CompanyExtendedFinDataFilter
    extends FilterData[CompanyExtendedFinData] {

    /**
      * Deletes the inconsistent entries that have data for some but not all parameters. It does that by
      * tail-recursively iterating through all the entries of each parameter, and tail-recursively constructing new
      * parameters. NaN values are already filtered out.
      */
    override def applyFilter(companyExtendedFinData: CompanyExtendedFinData): Option[CompanyExtendedFinData] = {
      val daily: CompanyDailyFinData = companyExtendedFinData.companyDailyFinData
      val companyDailyFinDataFiltered: Option[CompanyDailyFinData] = daily.filter   //returns consistent dailyFinData
      val companyExtendedWithDerivedParams: Option[CompanyExtendedFinData] =
        companyDailyFinDataFiltered.map(daily => companyExtendedFinData.copy(companyDailyFinData = daily))

      companyExtendedWithDerivedParams.flatMap { companyExtendedFinData =>
        val yearly = companyExtendedFinData.companyYearlyFinData
        val companyYearlyFinData: Option[CompanyYearlyFinData] = yearly.filter
        val companyDailyFinData = companyExtendedFinData.companyDailyFinData.filter
        val companyBMratio: Option[CompanyYearlyFinParameter] = companyExtendedFinData.companyBMratio
        val companyMarketValues: Option[CompanyYearlyFinParameter] = companyExtendedFinData.companyMarketValues
        val companySize: Option[CompanyYearlyFinParameter] = companyExtendedFinData.companySize
        val sym = companyExtendedFinData.companyDailyFinData.symbol

        val accrual  = companyYearlyFinData.map(_.accrual)


        val consistentYears: Set[Int] = (accrual |@| companyDailyFinDataFiltered |@| companyBMratio |@| companyMarketValues |@| companySize) {
          (yearly, daily, bMratio, marketVal, size) => {
            yearly.allCompanyEntriesOfOneYearlyParam.map(_.year).toSet
              .intersect(companyBMratio.map(_.perYearM.keySet).getOrElse(Set.empty[Int]))
              .intersect(companyMarketValues.map(_.perYearM.keySet).getOrElse(Set.empty[Int]))
              .intersect(companySize.map(_.perYearM.keySet).getOrElse(Set.empty[Int]))
              .intersect(daily.parameterDividends.groupedByYearM.keySet)
          }
        }.getOrElse(Set.empty[Int])

        val consistentDatesGivenYears: Set[DateTime] =
          companyDailyFinDataFiltered.map(_.parameterSUEs.allCompanyEntriesOfOneDailyParam.map(_.date).toSet
            .filter(date => consistentYears.contains(date.getYear))
          ).getOrElse(Set.empty[DateTime])

        val paramAccrual: Option[CompanyYearlyFinParameter] = companyYearlyFinData.map(_.accrual.filter(consistentYears)) match {
          case None => None
          case Some(yearlyParam) =>
            if(yearlyParam.allCompanyEntriesOfOneYearlyParam.isEmpty) None
            else Some(yearlyParam)
        }

        val paramBookVal = companyYearlyFinData.map(_.bookValue.filter(consistentYears)) match {
          case None => None
          case Some(yearlyParam) =>
            if(yearlyParam.allCompanyEntriesOfOneYearlyParam.isEmpty) None
            else Some(yearlyParam)
        }

        val paramROE = companyYearlyFinData.map(_.rOE.filter(consistentYears)) match {
          case None => None
          case Some(yearlyParam) =>
            if(yearlyParam.allCompanyEntriesOfOneYearlyParam.isEmpty) None
            else Some(yearlyParam)
        }

        val paramShares = companyYearlyFinData.map(_.shares.filter(consistentYears)) match {
          case None => None
          case Some(yearlyParam) =>
            if(yearlyParam.allCompanyEntriesOfOneYearlyParam.isEmpty) None
            else Some(yearlyParam)
        }

        val paramMarketVal: Option[CompanyYearlyFinParameter] = companyMarketValues.map(_.filter(consistentYears)) match {
          case None => None
          case Some(yearlyParam) =>
            if(yearlyParam.allCompanyEntriesOfOneYearlyParam.isEmpty) None
            else Some(yearlyParam)
        }

        val paramSize: Option[CompanyYearlyFinParameter] = companySize.map(_.filter(consistentYears)) match {
          case None => None
          case Some(yearlyParam) =>
            if(yearlyParam.allCompanyEntriesOfOneYearlyParam.isEmpty) None
            else Some(yearlyParam)
        }

        val paramDividend = companyDailyFinDataFiltered.map(_.parameterDividends.filter(consistentYears)) match {
          case None => None
          case Some(yearlyParam) =>
            if(yearlyParam.allCompanyEntriesOfOneDailyParam.isEmpty) None
            else Some(yearlyParam)
        }

        val paramQuotes = companyDailyFinDataFiltered.map(_.parameterQuotes
          .filter(consistentDatesGivenYears ++ consistentDatesGivenYears.map(_.plusDays(1))
          )
        )
        val paramSUE = companyDailyFinDataFiltered.map(_.parameterSUEs.filter(consistentYears)) match {
          case None => None
          case Some(yearlyParam) =>
            if(yearlyParam.allCompanyEntriesOfOneDailyParam.isEmpty) None
            else Some(yearlyParam)
        }

        (paramBookVal |@| paramShares |@| paramROE |@| paramAccrual |@| paramQuotes |@| paramDividend |@| paramSUE |@| paramMarketVal |@| companyBMratio |@| paramSize){
          (bookVal, shares, roe, accrual, quotes, dividend, sue, marketval, bmRatio, size) =>
            CompanyExtendedFinData(
              CompanyYearlyFinData(
                sym,
                bookVal,
                shares,
                roe,
                accrual
              ),
              CompanyDailyFinData(
                sym,
                dividend,
                quotes,
                sue
              ),
              Some(marketval),
              Some(bmRatio),
              Some(size)
            )
        }
      }
    }
  }





  /**
    * Produces the final combination of parameters for which there is at least one daily fin param for each year
    * for which there is a yearly param; at least one sentimentValue in same date with one Quote,
    * and in year for which there is daliy fin param.
    */
  implicit object CombinedCompanyParametersFilter extends FilterData[CombinedCompanyParameters] {
    override def applyFilter(allParameters: CombinedCompanyParameters): Option[CombinedCompanyParameters] = {

      val consistentFinancialData: Option[CompanyExtendedFinData] =
        allParameters.extendedFinData.filter

      val newsSentimentData: Option[CompanyNewsSentiment] = allParameters.newsSentiment

      val consistentSentiDatesIntersectQuotestDates: Set[DateTime] = {
        (newsSentimentData |@| consistentFinancialData) {
          (newsSentiment: CompanyNewsSentiment, financialData: CompanyExtendedFinData) => {
            financialData.companyDailyFinData.parameterQuotes.allCompanyEntriesOfOneDailyParam.map(_.date).toSet
              .intersect(newsSentiment.dates)
          }
        }.getOrElse(Set.empty)
      }

      val consistentSentimentData: Option[CompanyNewsSentiment] = allParameters.newsSentiment.map(_.filter(consistentSentiDatesIntersectQuotestDates))

      val consistentDailyFinData: Option[CompanyDailyFinData] =
        consistentFinancialData.map(_.companyDailyFinData.filter(consistentSentiDatesIntersectQuotestDates))

      val consistentYearlyFinData: Option[CompanyYearlyFinData] =
        consistentFinancialData.map(_.companyYearlyFinData.filter(consistentSentiDatesIntersectQuotestDates.map(_.getYear)))

      (consistentDailyFinData |@| consistentYearlyFinData |@| consistentSentimentData) {
        (dailyData, yearlyData, sentimentData) => {
          require(consistentSentimentData.forall(_.dates.diff(consistentSentiDatesIntersectQuotestDates).isEmpty))
          //          require(consistentDatesOfFinDataIntersectSentimentDates.forall(dailyData.parameterQuotes.allCompanyEntriesOfOneDailyParam.contains(_)))
          require(yearlyData.shares.allCompanyEntriesOfOneYearlyParam.map(_.year).toSet.diff(consistentSentiDatesIntersectQuotestDates.map(_.getYear)).isEmpty)
          require(yearlyData.accrual.allCompanyEntriesOfOneYearlyParam.map(_.year).toSet.diff(consistentSentiDatesIntersectQuotestDates.map(_.getYear)).isEmpty)
          require(yearlyData.rOE.allCompanyEntriesOfOneYearlyParam.map(_.year).toSet.diff(consistentSentiDatesIntersectQuotestDates.map(_.getYear)).isEmpty)
          require(yearlyData.bookValue.allCompanyEntriesOfOneYearlyParam.map(_.year).toSet.diff(consistentSentiDatesIntersectQuotestDates.map(_.getYear)).isEmpty)
          require(dailyData.parameterDividends.groupedByYearM.keySet.diff(consistentSentiDatesIntersectQuotestDates.map(_.getYear)).isEmpty)
          require(dailyData.parameterSUEs.groupedByYearM.keySet.diff(consistentSentiDatesIntersectQuotestDates.map(_.getYear)).isEmpty)
          require(dailyData.parameterQuotes.allCompanyEntriesOfOneDailyParam.map(_.date).toSet.diff(consistentSentiDatesIntersectQuotestDates ++ consistentSentiDatesIntersectQuotestDates.map(_.plusDays(1))).isEmpty)

          val companyExtended: Option[CompanyExtendedFinData] = CompanyExtendedFinData(
            yearlyData,
            dailyData,
            None,
            None,
            None
          ).deriveAdditionalFinParameters
//            .flatMap(_.filter)

          companyExtended.foreach(_.companyMarketValues.foreach(param => require(param.perYearM.keySet.diff(consistentSentiDatesIntersectQuotestDates.map(_.getYear)).isEmpty)))
          companyExtended.foreach(_.companyBMratio.foreach(param => require(param.perYearM.keySet.diff(consistentSentiDatesIntersectQuotestDates.map(_.getYear)).isEmpty)))
          companyExtended.foreach(_.companySize.foreach(param => require(param.perYearM.keySet.diff(consistentSentiDatesIntersectQuotestDates.map(_.getYear)).isEmpty)))

        }
          CompanyExtendedFinData(
            yearlyData,
            dailyData,
            None,
            None,
            None
          )
      }
        .flatMap(_.deriveAdditionalFinParameters())
//        .flatMap(_.filter)
        .map(extendedData => CombinedCompanyParameters(allParameters.symbol, extendedData, consistentSentimentData))
    }
  }
}
