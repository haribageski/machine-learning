package filters

import model.dailyFinancialParameters.{CompanyDailyFinData, CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import model.sentiment.{CompanyNewsSentiment, Sentiment}
import org.joda.time.DateTime
import FilterSyntax._
import scala.annotation.tailrec

object DefaultFilterParameterGivenDates {

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


}
