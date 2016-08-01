package filters

import model.{CombinedCompanyParameters, DateExtended}
import model.dailyFinancialParameters.{CompanyDailyFinData, CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import model.dailyNewsParameters.{CompanyAllNews, News}
import model.yearlyFinancialParameters.{CompanyYearlyExtendedFinData, CompanyYearlyFinData, CompanyYearlyFinDataEntry, CompanyYearlyFinParameter}
import FilterSyntax._
import model.sentiment.{CompanyNewsSentiment, Sentiment}
import org.joda.time.DateTime

import scala.annotation.tailrec

trait FilterParameterGivenYears[A] {
  def applyFilter(value: A, consistentYears: Set[Int]): A
}

trait FilterParameterGivenDates[A] {
  def applyFilter(value: A, consistentYears: Set[DateTime]): A
}

trait FilterData[A] {
  def applyFilter(value: A): A
}

object DefaultFilters {

  //TODO: Is this a good place for Validator? It is not really a filter.
  implicit object Validator {
    def validateValueInLines(indexes: Seq[Int])(line: List[String]): Boolean = {
      indexes.forall(index => line(index).toDouble != Double.NaN && line(index) != null)
    }
  }

  implicit object CompanyDailyFinParameterFilter extends FilterParameterGivenYears[CompanyDailyFinParameter] {

    override def applyFilter(companyDailyFinParameter: CompanyDailyFinParameter, consistentYears: Set[Int]):
    CompanyDailyFinParameter = {

      val symbol = companyDailyFinParameter.symbol

      @tailrec
      def recursivelyIterateConsistentYears(allEntries: List[CompanyDailyFinDataEntry],
                                            dailyFinParam: CompanyDailyFinParameter): CompanyDailyFinParameter = {
        allEntries match {
          case Nil =>
            val listOfVals = dailyFinParam.allCompanyEntriesOfOneDailyParam.reverse
            dailyFinParam.copy(allCompanyEntriesOfOneDailyParam = listOfVals)

          case head :: tail =>
            if (consistentYears.contains(head.date.getYear)) {
              val paramWithAddedEntry = dailyFinParam.addEntry(head)
              recursivelyIterateConsistentYears(
                tail,
                paramWithAddedEntry
              )
            }
            else
              recursivelyIterateConsistentYears(
                tail,
                dailyFinParam
              )
        }
      }
      recursivelyIterateConsistentYears(
        companyDailyFinParameter.allCompanyEntriesOfOneDailyParam, CompanyDailyFinParameter(symbol)
      )
    }
  }


  implicit object CompanyDailyFinParameterFilterGivenDates extends FilterParameterGivenDates[CompanyDailyFinParameter] {

    override def applyFilter(companyDailyFinParameter: CompanyDailyFinParameter, consistentDatess: Set[DateTime]):
    CompanyDailyFinParameter = {

      val symbol = companyDailyFinParameter.symbol

      @tailrec
      def recursivelyIterateConsistentYears(allEntries: List[CompanyDailyFinDataEntry],
                                            dailyFinParam: CompanyDailyFinParameter): CompanyDailyFinParameter = {
        allEntries match {
          case Nil =>
            val listOfVals = dailyFinParam.allCompanyEntriesOfOneDailyParam.reverse
            dailyFinParam.copy(allCompanyEntriesOfOneDailyParam = listOfVals)

          case head :: tail =>
            if (consistentDatess.contains(head.date)) {
              val paramWithAddedEntry = dailyFinParam.addEntry(head)
              recursivelyIterateConsistentYears(
                tail,
                paramWithAddedEntry
              )
            }
            else
              recursivelyIterateConsistentYears(
                tail,
                dailyFinParam
              )
        }
      }
      recursivelyIterateConsistentYears(
        companyDailyFinParameter.allCompanyEntriesOfOneDailyParam, CompanyDailyFinParameter(symbol)
      )
    }
  }


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

  implicit object CompanyYearlyFinParameterFilter
    extends FilterParameterGivenYears[CompanyYearlyFinParameter] {

    /**
      * Provided the consistent years, it creates new CompanyYearlyFinParameter from the current one, that is consistent
      * with the provided years. The parameters allCompanyEntries and companyYearlyFinParameter are only used inside its
      * implementation. Only consistentYears should be provided as an input parameter.
      *
      * @param consistentYears - hose are the years we are sure to be consistent
      * @return
      */
    override def applyFilter(companyYearlyFinParameter: CompanyYearlyFinParameter, consistentYears: Set[Int]) = {
      val emptyCompanyYearlyFinParameter: CompanyYearlyFinParameter =
        CompanyYearlyFinParameter(companyYearlyFinParameter.symbol, None, None, Map.empty, List())

      val allEntries: List[CompanyYearlyFinDataEntry] = companyYearlyFinParameter.allCompanyEntriesOfOneYearlyParam

      val filteredEntries = allEntries.filter(entry => consistentYears.contains(entry.year))

      emptyCompanyYearlyFinParameter.addEntries(filteredEntries.reverse)
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


  implicit object CompanyYearlyFinDataFilterFromConsistentYears extends FilterParameterGivenYears[CompanyYearlyFinData] {
    override def applyFilter(finData: CompanyYearlyFinData, consistentYears: Set[Int]): CompanyYearlyFinData = {
      CompanyYearlyFinData(
        finData.symbol,
        finData.bookValue.filter(consistentYears),
        finData.shares.filter(consistentYears),
        finData.rOE.filter(consistentYears),
        finData.accrual.filter(consistentYears)
      )
    }
  }


  implicit object CompanyDailyFinDataFilterFromConsistentYears extends FilterParameterGivenYears[CompanyDailyFinData] {
    override def applyFilter(finData: CompanyDailyFinData, consistentYears: Set[Int]): CompanyDailyFinData = {
      CompanyDailyFinData(
        finData.symbol,
        finData.parameterDividends.filter(consistentYears),
        finData.parameterQuotes.filter(consistentYears),
        finData.parameterSUEs.filter(consistentYears)
      )
    }
  }

  implicit object CompanyDailyFinDataFilterFromConsistentDates extends FilterParameterGivenDates[CompanyDailyFinData] {
    override def applyFilter(value: CompanyDailyFinData, consistentDates: Set[DateTime]): CompanyDailyFinData = {
            CompanyDailyFinData(
              value.symbol,
              value.parameterDividends.filter(consistentDates),
              value.parameterQuotes.filter(consistentDates),
              value.parameterSUEs.filter(consistentDates)
            )
          }
  }

  implicit object CompanyYearlyExtendedFinDataFilterFromConsistentYears extends FilterParameterGivenYears[CompanyYearlyExtendedFinData] {
    override def applyFilter(finData: CompanyYearlyExtendedFinData, consistentYears: Set[Int]): CompanyYearlyExtendedFinData =
      CompanyYearlyExtendedFinData(
        finData.companyYearlyFinData.filter(consistentYears),
        finData.companyDailyFinData.filter(consistentYears),
        finData.companyMarketValues.map(_.filter(consistentYears)),
        finData.companyBMratio.map(_.filter(consistentYears)),
        finData.companySize.map(_.filter(consistentYears))
      )
  }

  implicit object CompanyAllNewsFilter extends FilterParameterGivenYears[CompanyAllNews] {

    /**
      * Provided the consistent years, creates new CompanyAllNews from the current one consistent with the provided years.
      */
    override def applyFilter(allNews: CompanyAllNews, consistentYears: Set[Int]): CompanyAllNews = {
      val filteredNews = allNews.news.filter(news => consistentYears.contains(news.dateOfNews.getYear))
      CompanyAllNews(allNews.symbol, filteredNews)
    }
  }


  implicit object CompanyNewsSentimentFilter extends FilterParameterGivenDates[CompanyNewsSentiment] {

    /**
      * Provided the consistent dates, check if there is financial parameter in the same date and in one day later.
      */
    override def applyFilter(companySentiments: CompanyNewsSentiment, consistentDates: Set[DateTime]): CompanyNewsSentiment = {

      val consistentAvgTitleM: Map[DateTime, Sentiment] =
        companySentiments.avgSentiPerDateTitle.filterKeys{
          dateE => consistentDates.contains(dateE) && consistentDates.contains(dateE.plusDays(1))
        }
      val consistentAvgDescriptM: Map[DateTime, Sentiment] =
        companySentiments.avgSentiPerDateDescript.filterKeys{
          dateE => consistentDates.contains(dateE) && consistentDates.contains(dateE.plusDays(1))
        }

      CompanyNewsSentiment(companySentiments.sym, consistentAvgTitleM, consistentAvgDescriptM)
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
