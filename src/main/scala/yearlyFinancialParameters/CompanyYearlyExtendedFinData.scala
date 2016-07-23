package yearlyFinancialParameters

import dailyFinancialParameters._
import utils.SymYear
import scala.annotation.tailrec
import scala.collection.immutable.TreeSet


//TODO: marketvalue to be part of companyYearly
case class CompanyYearlyExtendedFinData(companyYearlyFinData: CompanyYearlyFinData,
                                        companyDailyFinData: CompanyDailyFinData,
                                        companyMarketValues: Option[CompanyYearlyFinParameter] = None,
                                        companyBMratio: Option[CompanyYearlyFinParameter] = None,
                                        companySize: Option[CompanyYearlyFinParameter] = None
                                       ) {
    /**
      * Given the yearly parameters in CompanyYearlyFinData and the daily parameters in companyDailyFinData,
      * it finds parameterMarketValues, parameterBMratio, and parameterSize. Then it returns
      * new CompanyYearlyExtendedFinData with the three derived parameters included.
      * The entries will be a set of SymDates that is the intersection of all the yearly parameters entries and all the
      * daily parameters.
    */
  def deriveAdditionalFinParameters(): CompanyYearlyExtendedFinData = {
    val symYears = companyYearlyFinData.bookValue.perYearM.keySet
    val sym = companyYearlyFinData.symbol

    val marketValsToAdd: List[CompanyYearlyFinDataEntry] = findMarketValuesToAdd(symYears = symYears)


    val bMratiosToAdd: List[CompanyYearlyFinDataEntry] = marketValsToAdd.flatMap(marketValEntry => {
      val bookValOpt: Option[CompanyYearlyFinDataEntry] =
        companyYearlyFinData.bookValue.perYearM.get(marketValEntry.symYear)
      bookValOpt.map(book =>
        marketValEntry.copy(value = Math.log10(book.value / marketValEntry.value))  //division is safe
      )
    })

    val sizeValsYearlyToAdd: List[CompanyYearlyFinDataEntry] = marketValsToAdd.map(marketValEntry =>
        marketValEntry.copy(value = Math.log10(marketValEntry.value))
      )

    val companyMarketValuesWithAddedNewValues: Option[CompanyYearlyFinParameter] =
      companyMarketValues match {
        case Some(marketVal) => Some(marketVal.addEntries(marketValsToAdd))
        case None => Some(CompanyYearlyFinParameter(sym).addEntries(marketValsToAdd))
      }

    val companyBMratioWithAddedNewValues: Option[CompanyYearlyFinParameter] =
      companyBMratio match {
        case Some(bMratio) => Some(bMratio.addEntries(bMratiosToAdd))
        case None => Some(CompanyYearlyFinParameter(sym).addEntries(bMratiosToAdd))
      }

    val companySizeWithAddedNewValues: Option[CompanyYearlyFinParameter] =
      companySize match {
        case Some(cSize) => Some(cSize.addEntries(sizeValsYearlyToAdd))
        case None => Some(CompanyYearlyFinParameter(sym).addEntries(sizeValsYearlyToAdd))
      }

    CompanyYearlyExtendedFinData(
      companyYearlyFinData,
      companyDailyFinData,
      companyMarketValuesWithAddedNewValues,
      companyBMratioWithAddedNewValues,
      companySizeWithAddedNewValues
    )
  }


  /**
    * Finds nonzero MarketValue using average per year quotes and yearly share for all years from the input parameter.
    * If for some year is not possible, then the returning list will not contain any MarketValue for that year.
    */
  @tailrec
  private def findMarketValuesToAdd(marketVals: List[CompanyYearlyFinDataEntry] = Nil, symYears: Set[SymYear]):
  List[CompanyYearlyFinDataEntry] = {
    if(symYears.isEmpty)    marketVals
    else {
        val head = symYears.head
        val sym = head.sym
        val year = head.year

        val avgPerYQuotesClosingPrice: Option[Double] =
          getAvgQuotesClosingPricePerY(companyDailyFinData.parameterQuotes, year)

        val yearlyShare: Option[Double] =
          companyYearlyFinData.shares.perYearM.get(head).map(_.value)

        val marketValues: Option[Double] = for {
          avgQuote <- avgPerYQuotesClosingPrice
          share <- yearlyShare
        } yield avgQuote * share

        val nonZeroMarketValues: Option[Double] = marketValues.filter(_ != 0)

        nonZeroMarketValues match {
          case Some(v) => findMarketValuesToAdd(
            CompanyYearlyFinDataEntry(sym, v, year) :: marketVals, symYears.tail
          )
          case None => findMarketValuesToAdd(marketVals, symYears.tail)
        }
    }
  }


  private def getAvgQuotesClosingPricePerY(quotes: CompanyDailyFinParameter, year: Int): Option[Double] = {
    val treeSetDailyEntriesOpt: Option[TreeSet[CompanyDailyFinDataEntry]] =
      quotes.groupedByYearM.get(year)

    val totalValOpt: Option[Double] = treeSetDailyEntriesOpt.map {
      tree =>
        tree.foldLeft(0.0)((acc, entry: CompanyDailyFinDataEntry) => acc + entry.value)
    }

    // TODO: Use ScalaZ to refactore the code below
    treeSetDailyEntriesOpt.flatMap { tree =>
      totalValOpt.map(total => total / tree.size)
    }
  }
}
