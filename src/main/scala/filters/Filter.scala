package filters

import model.{CombinedCompanyParameters, DateExtended}
import model.dailyFinancialParameters.{CompanyDailyFinData, CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import model.dailyNewsParameters.{CompanyAllNews, News}
import model.yearlyFinancialParameters.{CompanyYearlyExtendedFinData, CompanyYearlyFinData, CompanyYearlyFinDataEntry, CompanyYearlyFinParameter}
import FilterSyntax._
import model.sentiment.{CompanyNewsSentiment, Sentiment}

import scala.annotation.tailrec

trait FilterParameter[A] {
  def applyFilter(value: A, consistentYears: Set[Int]): A
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

  implicit object CompanyDailyFinParameterFilter extends FilterParameter[CompanyDailyFinParameter] {

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
            if (consistentYears.contains(head.date.dateExtended.getYear)) {
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
        parameterDividends.allCompanyEntriesOfOneDailyParam.map(_.date.dateExtended.getYear)
          .intersect(parameterQuotes.allCompanyEntriesOfOneDailyParam.map(_.date.dateExtended.getYear))
          .intersect(parameterSUEs.allCompanyEntriesOfOneDailyParam.map(_.date.dateExtended.getYear))

      val dividendsToAdd = parameterDividends.allCompanyEntriesOfOneDailyParam.filter(param =>
        intersectAllThreeDailyParamsYears.contains(param.date.dateExtended.getYear))
      val quotesToAdd = parameterQuotes.allCompanyEntriesOfOneDailyParam.filter(param =>
        intersectAllThreeDailyParamsYears.contains(param.date.dateExtended.getYear))
      val sUEsToAdd = parameterSUEs.allCompanyEntriesOfOneDailyParam.filter(param =>
        intersectAllThreeDailyParamsYears.contains(param.date.dateExtended.getYear))

      val synchronizedParameterDividends = CompanyDailyFinParameter(symbol).addEntries(dividendsToAdd)
      val synchronizedParameterQuotes = CompanyDailyFinParameter(symbol).addEntries(quotesToAdd)
      val synchronizedparameterSUEs = CompanyDailyFinParameter(symbol).addEntries(sUEsToAdd)

      CompanyDailyFinData(symbol, synchronizedParameterDividends, synchronizedParameterQuotes, synchronizedparameterSUEs)
    }
  }

  implicit object CompanyYearlyFinParameterFilter
    extends FilterParameter[CompanyYearlyFinParameter] {

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


      @tailrec
      def recursivelyIterateConsistentYears(allEntries: List[CompanyYearlyFinDataEntry],
                                            yearlyFinParameter: CompanyYearlyFinParameter): CompanyYearlyFinParameter =
        allEntries match {
          case Nil =>
            val listOfVals = yearlyFinParameter.allCompanyEntriesOfOneYearlyParam.reverse
            yearlyFinParameter.copy(allCompanyEntriesOfOneYearlyParam = listOfVals)

          case h :: t =>
            if (consistentYears.contains(h.symYear.year))
              recursivelyIterateConsistentYears(
                t,
                yearlyFinParameter.addEntry(h)
              )
            else
              recursivelyIterateConsistentYears(
                t,
                yearlyFinParameter
              )
        }
      recursivelyIterateConsistentYears(
        companyYearlyFinParameter.allCompanyEntriesOfOneYearlyParam, emptyCompanyYearlyFinParameter
      )
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
      val consistentEntries: Set[Int] = companyBMratio match {
        case None => Set.empty
        case Some(set) => set.perYearM.keySet.map(_.year)
      }

      val paramAccrual: CompanyYearlyFinParameter = CompanyYearlyFinParameterFilter.applyFilter(
        companyYearlyFinData.accrual,
        consistentEntries
      )
      val paramBookVal = CompanyYearlyFinParameterFilter.applyFilter(
        companyYearlyFinData.bookValue,
        consistentEntries
      )
      val paramROE = CompanyYearlyFinParameterFilter.applyFilter(
        companyYearlyFinData.rOE, consistentEntries
      )
      val paramShares = CompanyYearlyFinParameterFilter.applyFilter(
        companyYearlyFinData.shares, consistentEntries
      )

      val paramMarketVal: Option[CompanyYearlyFinParameter] =
        companyMarketValues.map(CompanyYearlyFinParameterFilter.applyFilter(
          _, consistentEntries
        ))
      val paramSize: Option[CompanyYearlyFinParameter] =
        companySize.map(CompanyYearlyFinParameterFilter.applyFilter(
          _, consistentEntries
        ))

      val paramDividend = CompanyDailyFinParameterFilter.applyFilter(
        companyDailyFinData.parameterDividends, consistentEntries
      )
      val paramQuotes = CompanyDailyFinParameterFilter.applyFilter(
        companyDailyFinData.parameterQuotes, consistentEntries
      )
      val paramSUE = CompanyDailyFinParameterFilter.applyFilter(
        companyDailyFinData.parameterSUEs, consistentEntries
      )

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


  implicit object CompanyYearlyFinDataFilterFromConsistentYears extends FilterParameter[CompanyYearlyFinData] {
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


  implicit object CompanyDailyFinDataFilterFromConsistentYears extends FilterParameter[CompanyDailyFinData] {
    override def applyFilter(finData: CompanyDailyFinData, consistentYears: Set[Int]): CompanyDailyFinData = {
      CompanyDailyFinData(
        finData.symbol,
        finData.parameterDividends.filter(consistentYears),
        finData.parameterQuotes.filter(consistentYears),
        finData.parameterSUEs.filter(consistentYears)
      )
    }
  }



    implicit object CompanyYearlyExtendedFinDataFilterFromConsistentYears extends FilterParameter[CompanyYearlyExtendedFinData] {
    override def applyFilter(finData: CompanyYearlyExtendedFinData, consistentYears: Set[Int]): CompanyYearlyExtendedFinData =
      CompanyYearlyExtendedFinData(
        finData.companyYearlyFinData.filter(consistentYears),
        finData.companyDailyFinData.filter(consistentYears),
        finData.companyMarketValues.map(_.filter(consistentYears)),
        finData.companyBMratio.map(_.filter(consistentYears)),
        finData.companySize.map(_.filter(consistentYears))
      )
  }

  implicit object CompanyAllNewsFilter extends FilterParameter[CompanyAllNews] {

    /**
      * Provided the consistent years, creates new CompanyAllNews from the current one consistent with the provided years.
      */
    override def applyFilter(allNews: CompanyAllNews, consistentYears: Set[Int]): CompanyAllNews = {
      val filteredNews = allNews.news.filter(news => consistentYears.contains(news.dateOfNews.dateExtended.getYear))
      CompanyAllNews(allNews.symbol, filteredNews)
    }
  }


  implicit object CompanyNewsSentimentFilter extends FilterParameter[CompanyNewsSentiment] {

    /**
      * Provided the consistent years, creates new CompanyAllNews from the current one consistent with the provided years.
      */
    override def applyFilter(companySentiments: CompanyNewsSentiment, consistentYears: Set[Int]): CompanyNewsSentiment = {
      val consistentAvgTitleM: Map[DateExtended, Sentiment] =
        companySentiments.avgSentiPerDateTitle.filterKeys(dateE => consistentYears.contains(dateE.dateExtended.getYear))
      val consistentAvgDescriptM: Map[DateExtended, Sentiment] =
        companySentiments.avgSentiPerDateDescript.filterKeys(dateE => consistentYears.contains(dateE.dateExtended.getYear))

      CompanyNewsSentiment(companySentiments.sym, consistentAvgTitleM, consistentAvgDescriptM)
    }
  }


  implicit object CombinedCompanyParametersFilter extends FilterData[CombinedCompanyParameters] {
    override def applyFilter(allParameters: CombinedCompanyParameters): CombinedCompanyParameters = {
      val consistentFinancialData: CompanyYearlyExtendedFinData = allParameters.yearlyExtendedFinData.filter
      val concistentCombinedCompanyParametersYears: Set[Int] =
        allParameters.newsSentiment.avgSentiPerDateDescript.keySet.map(_.dateExtended.getYear)
        .intersect(consistentFinancialData.companyYearlyFinData.accrual.perYearM.keySet.map(_.year))

      CombinedCompanyParameters(
        allParameters.symbol,
        allParameters.yearlyExtendedFinData.filter(concistentCombinedCompanyParametersYears),
        allParameters.newsSentiment.filter(concistentCombinedCompanyParametersYears)
      )
    }
  }
}
