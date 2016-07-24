package utils.readers

import model.DateExtended
import model.dailyFinancialParameters.{CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import utils.readers.ReadableColumnsDefaults.ColumnsReader

object ReadableParameterDefaults {

  implicit object CompanyDailyFinParameterReader extends ReadableParameter[CompanyDailyFinParameter] {
    /**
      * The files to read a parameter from are with lines of the following structure:
      * symbol /t date /t value
      * Remark: Not tail recursive.
      */
    override def readParameterFromFile(filePath: String, symbol: String, indexOfValue: Int): CompanyDailyFinParameter = {
      val readData: List[List[String]] = ColumnsReader.readColumnsFromFile(filePath)

      val filteredInput: List[List[String]] =
        readData.filter(
          CompanyDailyFinParameter.validateValueInLine(indexOfValue)
        )

      val entries: List[CompanyDailyFinDataEntry] =
        filteredInput.map(line =>
          CompanyDailyFinDataEntry(symbol, line(indexOfValue).toDouble, DateExtended(line(1)))
        )

      val newCompanyDailyFinData = CompanyDailyFinParameter(symbol)
      newCompanyDailyFinData.addEntries(entries)
    }

    def readDividendFromFile(symbol: String) =
      readParameterFromFile("resources/dividends/" + symbol + ".txt", symbol, 2)

    def readQuotesFromFile(symbol: String) =
      readParameterFromFile("resources/quotes-prices/" + symbol + ".txt", symbol, 3)

    def readEarningSurpriseFromFile(symbol: String) =
      readParameterFromFile("resources/earning-surprises/" + symbol + ".txt", symbol, 2)
  }

}
