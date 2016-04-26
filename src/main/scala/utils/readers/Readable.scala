package utils.readers

import dailyFinancialParameters.{CompanyDailyFinData, CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import utils.DateExtended

trait ReadableParameter[A] {
  def readParameterFromFile(filePath: String, symbol: String, indexOfValue: Int): A
}

trait Readable[A] {
  def readDataFromFile(symbol: String): A
}

trait ReadableColumns[F[F[String]]] {
  def readColumnsFromFile(filePath: String): F[F[String]]
}

object ReadableDefaults {
  implicit object CompanyDailyFinParameterReader extends ReadableParameter[CompanyDailyFinParameter] {
    /**
      * The files to read the dividends from are with lines of the following structure:
      * symbol /t date /t value
      * Remark: Not tail recursive.
      */
    override def readParameterFromFile(filePath: String, symbol: String, indexOfValue: Int): CompanyDailyFinParameter = {
      import reading_data_from_file.ReadColumns

      val readData: List[List[String]] = ReadColumns.readColumns(filePath)

      val filteredInput: List[List[String]] =
        readData.filter(
          CompanyDailyFinParameter.validateValueInLine(indexOfValue)
        )

      val entries: List[CompanyDailyFinDataEntry] =
        filteredInput.map(line =>
          CompanyDailyFinDataEntry(symbol, line(indexOfValue).toDouble, DateExtended(line(1)))
        )

      val newCompanyDailyFinData = CompanyDailyFinParameter(symbol)

      val entriesRead: CompanyDailyFinParameter = entries.foldLeft(newCompanyDailyFinData)((acc, entryToAdd) => {
        acc.addEntry(entryToAdd)
      })
      entriesRead
    }

    def readDividendFromFile(symbol: String) =
      readParameterFromFile("resources/dividends/" + symbol + ".txt", symbol, 2)

    def readQuotesFromFile(symbol: String) =
      readParameterFromFile("resources/quotes-prices/" + symbol + ".txt", symbol, 3)

    def readEarningSurpriseFromFile(symbol: String) =
      readParameterFromFile("resources/earning-surprises/" + symbol + ".txt", symbol, 2)
  }

  implicit object CompanyDailyFinDataReader extends Readable[CompanyDailyFinData] {
    /**
      * Provided a symbol, uses CompanyDailyFinParameterReader.readParameterFromFile() to read
      * the dividends/quotes/SUEs and return new CompanyDailyFinData with all the read entries.
      */
    override def readDataFromFile(symbol: String): CompanyDailyFinData = {
      import utils.filters.DefaultFilters.CompanyDailyFinDataFilter
      import utils.filters.FilterSyntax.FilterOps


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
        .filter(CompanyDailyFinDataFilter) /*it filters out the data for which there is one of the three
       parameters that has no entry in that year*/
    }
  }

  implicit object ColumnsReader extends ReadableColumns[List[List[String]]]
}
