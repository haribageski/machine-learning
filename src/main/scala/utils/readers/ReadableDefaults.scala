package utils.readers

import analyzers.SentimentAnalyzer
import filters.DefaultFilterData.CompanyDailyFinDataFilter
import model.dailyFinancialParameters.{CompanyDailyFinData, CompanyDailyFinParameter}
import model.dailyNewsParameters.{CompanyAllNews, News}
import model.{CombinedCompanyParameters, DateExtended}
import model.yearlyFinancialParameters.{CompanyExtendedFinData, CompanyYearlyFinData, CompanyYearlyFinDataEntry, CompanyYearlyFinParameter}
import utils.readers.ReadableColumnsDefaults.ColumnsReader
import utils.readers.ReadableParameterDefaults.CompanyDailyFinParameterReader
import filters.FilterSyntax.FilterOps
import filters.Validator.Validator

object ReadableDefaults {

  implicit object CompanyDailyFinDataReader extends Readable[CompanyDailyFinData] {
    /**
      * Provided a symbol, uses CompanyDailyFinParameterReader.readParameterFromFile() to read
      * the dividends/quotes/SUEs and return new CompanyDailyFinData with all the read entries.
      */
    override def readDataFromFile(symbol: String): CompanyDailyFinData = {

      val companyDailyFinParameter = CompanyDailyFinParameter(symbol)
      CompanyDailyFinData(
        symbol,
        CompanyDailyFinParameterReader.readParameterFromFile(
          "resources/dividends/" + symbol + ".txt",
          symbol,
          indexOfValue = 2
        ),
        CompanyDailyFinParameterReader.readParameterFromFile(
          "resources/quotes-prices/" + symbol + ".txt",
          symbol,
          indexOfValue = 3
        ),
        CompanyDailyFinParameterReader.readParameterFromFile(
          "resources/earning-surprises/" + symbol + ".txt",
          symbol,
          indexOfValue = 2
        )
      )
    }
  }

  /**
    * The files to read the yearly parameters from are with lines of the following structure:
    * symbol /t date /t parameter1 /t parameter2 /t parameter3 /t parameter4
    */
  implicit object CompanyYearlyFinDataReader extends Readable[CompanyYearlyFinData] {
    override def readDataFromFile(symbol: String): CompanyYearlyFinData = {

      //TODO: Get filePath in other way (from config)
      val filePath = "resources/yearly-fin-parameters/" + symbol + ".txt"

      val indexesOfValues = Seq(2, 3, 4, 5)
      val inputDataFromFile: List[List[String]] = ColumnsReader.readColumnsFromFile(filePath)

      val filteredInput: List[List[String]] = inputDataFromFile.filter(Validator.validateValueInLines(indexesOfValues))
      val filteredZeroShares = filteredInput.filter(line => line(3).toInt != 0)

      //TODO: The followint mappings can be combined and done in one iteration
      val entriesForParameterBookVal: List[CompanyYearlyFinDataEntry] =
        filteredZeroShares.map(
          line => CompanyYearlyFinDataEntry(
            symbol, line(2).toDouble, DateExtended(line(1)).dateExtended.getYear
          )
        )
      val entriesForParameterShares: List[CompanyYearlyFinDataEntry] =
        filteredZeroShares.map(
          line => CompanyYearlyFinDataEntry(
            symbol, line(3).toDouble, DateExtended(line(1)).dateExtended.getYear
          )
        )
      val entriesForParameterROEs: List[CompanyYearlyFinDataEntry] =
        filteredZeroShares.map(
          line => CompanyYearlyFinDataEntry(
            symbol, line(4).toDouble, DateExtended(line(1)).dateExtended.getYear
          )
        )
      val entriesForParameterAccruals: List[CompanyYearlyFinDataEntry] =
        filteredZeroShares.map(
          line => CompanyYearlyFinDataEntry(
            symbol, line(5).toDouble, DateExtended(line(1)).dateExtended.getYear
          )
        )

      val emptyBookValue = CompanyYearlyFinParameter(symbol)
      val emptyShares = CompanyYearlyFinParameter(symbol)
      val emptyROE = CompanyYearlyFinParameter(symbol)
      val emptyAccrual = CompanyYearlyFinParameter(symbol)

      val allBookValues: CompanyYearlyFinParameter =
        entriesForParameterBookVal.foldLeft(emptyBookValue)((acc, entryYoAdd) => acc.addEntry(entryYoAdd))
      val allShares: CompanyYearlyFinParameter =
        entriesForParameterShares.foldLeft(emptyShares)((acc, entryToAdd) => acc.addEntry(entryToAdd))
      val allROEs: CompanyYearlyFinParameter =
        entriesForParameterROEs.foldLeft(emptyROE)((acc, entryYoAdd) => acc.addEntry(entryYoAdd))
      val allAccruals: CompanyYearlyFinParameter =
        entriesForParameterAccruals.foldLeft(emptyAccrual)((acc, entryYoAdd) => acc.addEntry(entryYoAdd))

      new CompanyYearlyFinData(
        symbol,
        allBookValues,
        allShares,
        allROEs,
        allAccruals
      )
    }
  }

  implicit object CompanyNewsReader extends Readable[CompanyAllNews] {
    override def readDataFromFile(symbol: String): CompanyAllNews = {
      //TODO: Get filePath in other way (from config)
      val filePath = "resources/news/google_news_" + symbol + ".txt"

      val inputDataFromFile: List[List[String]] = ColumnsReader.readColumnsFromFile(filePath)
      val allCompanyNews = inputDataFromFile.map {line =>
        val date = DateExtended(line(1))
        News(symbol, date.dateExtended, date.dateExtended.getYear, line(2), line(3))
      }
      CompanyAllNews(symbol, allCompanyNews)
    }
  }

  implicit object CombinedCompanyParametersReader extends Readable[CombinedCompanyParameters] {
    override def readDataFromFile(sym: String): CombinedCompanyParameters = {
      val allCompanyNews = CompanyNewsReader.readDataFromFile(sym)

      CombinedCompanyParameters(
        sym,
        CompanyExtendedFinData(
          CompanyYearlyFinDataReader.readDataFromFile(sym),
          CompanyDailyFinDataReader.readDataFromFile(sym)
        ),
        SentimentAnalyzer.evaluateSentiOfAllCompanyNews(allCompanyNews)
      )
    }
  }
}
