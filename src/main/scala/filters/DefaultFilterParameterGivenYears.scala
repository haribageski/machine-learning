package filters

import model.dailyFinancialParameters.{CompanyDailyFinData, CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import model.dailyNewsParameters.CompanyAllNews
import model.yearlyFinancialParameters.{CompanyExtendedFinData, CompanyYearlyFinData, CompanyYearlyFinDataEntry, CompanyYearlyFinParameter}
import FilterSyntax._
import scala.annotation.tailrec

object DefaultFilterParameterGivenYears {

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


  implicit object CompanyYearlyExtendedFinDataFilterFromConsistentYears extends FilterParameterGivenYears[CompanyExtendedFinData] {
    override def applyFilter(finData: CompanyExtendedFinData, consistentYears: Set[Int]): CompanyExtendedFinData =
      CompanyExtendedFinData(
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
}
