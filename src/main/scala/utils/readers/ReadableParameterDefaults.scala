package utils.readers

import model.DateExtended
import model.dailyFinancialParameters.{CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import utils.readers.ReadableColumnsDefaults.ColumnsReader
import scalaz.Validation

object ReadableParameterDefaults {

  implicit object CompanyDailyFinParameterReader extends ReadableParameter[CompanyDailyFinParameter] {

    type ErrorValidation[A] = Validation[String, A]

    /**
      * The files to read a parameter from are with lines of the following structure:
      * symbol /t date /t value
      * Remark: Not tail recursive.
      */
    override def readParameterFromFile(filePath: String, symbol: String, indexOfValue: Int): ErrorValidation[CompanyDailyFinParameter] = {
      val readDataE: Validation[String, List[List[String]]] = ColumnsReader.readColumnsFromFile(filePath)
      readDataE.map { readData =>
        val filteredInput: List[List[String]] =
          readData.filter(
            CompanyDailyFinParameter.validateValueInLine(indexOfValue)
          )

        val entries: List[CompanyDailyFinDataEntry] =
          filteredInput.map(line =>
            CompanyDailyFinDataEntry(symbol, line(indexOfValue).toDouble, DateExtended.fromString(line(1)))
          )

        val newCompanyDailyFinData = CompanyDailyFinParameter(symbol)
        newCompanyDailyFinData.addEntries(entries)
      }
    }
    def readDividendFromFile(symbol: String) =
      readParameterFromFile("resources/dividends/" + symbol + ".txt", symbol, 2)

    def readQuotesFromFile(symbol: String) =
      readParameterFromFile("resources/quotes-prices/" + symbol + ".txt", symbol, 3)

    def readEarningSurpriseFromFile(symbol: String) =
      readParameterFromFile("resources/earning-surprises/" + symbol + ".txt", symbol, 2)
  }
}
