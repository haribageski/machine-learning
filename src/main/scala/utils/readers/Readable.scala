package utils.readers

import dailyFinancialParameters.{CompanyDailyFinData, CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import utils.DateExtended
import yearlyFinancialParameters.{CompanyYearlyFinData, CompanyYearlyFinParameter, CompanyYearlyFinDataEntry}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source._
import better.files._
import java.io.{File => JFile}

trait ReadableParameter[A] {
  def readParameterFromFile(filePath: String, symbol: String, indexOfValue: Int): A
}

trait Readable[A] {
  def readDataFromFile(symbol: String): A
}

trait ReadableColumns {
  def readColumnsFromFile(filePath: String): List[List[String]]
}

object ReadableDefaults {
  implicit object CompanyDailyFinParameterReader extends ReadableParameter[CompanyDailyFinParameter] {
    /**
      * The files to read the dividends from are with lines of the following structure:
      * symbol /t date /t value
      * Remark: Not tail recursive.
      * Test: None
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

      val entriesRead: CompanyDailyFinParameter = entries.foldLeft(newCompanyDailyFinData)((acc, entryToAdd) => {
        acc.addEntry(entryToAdd)
      })
      entriesRead
    }

    /**
      * Test: utils.ReadersTest
      *
      * @param symbol
      * @return
      */
    def readDividendFromFile(symbol: String) =
      readParameterFromFile("resources/dividends/" + symbol + ".txt", symbol, 2)

    /**
      * Test: utils.ReadersTest
      *
      * @param symbol
      * @return
      */
    def readQuotesFromFile(symbol: String) =
      readParameterFromFile("resources/quotes-prices/" + symbol + ".txt", symbol, 3)

    /**
      * Test: utils.ReadersTest
      *
      * @param symbol
      * @return
      */
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

  /**
    * The files to read the yearly parameters from are with lines of the following structure:
    * symbol /t date /t parameter1 /t parameter2 /t parameter3 /t parameter4
    */
  implicit object CompanyYearlyFinDataReader extends Readable[CompanyYearlyFinData] {
    override def readDataFromFile(symbol: String): CompanyYearlyFinData = {
//      import utils.readers.ReadableDefaults.ColumnsReader
      import utils.filters.DefaultFilters.Validator._

      //TODO: Get filePath in other way (from config)
      val filePath = "resources/yearly-fin-parameters/" + symbol + ".txt"

      val indexesOfValues = Seq(2,3,4,5)
      val inputDataFromFile: List[List[String]] = ColumnsReader.readColumnsFromFile(filePath)

      val filteredInput: List[List[String]] = inputDataFromFile.filter(validateValueInLines(indexesOfValues))
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


  implicit object ColumnsReader extends ReadableColumns {
    def readColumnsFromFile(filePath: String): List[List[String]] = {
      val lines: Traversable[String] = readFile(filePath)
      findColumnsFromInputLines(lines, ListBuffer.empty[List[String]])
    }

    private def readFile(filePath: String): Traversable[String] = {
      val file = File(filePath)
      file.lines
    }

    /**
      * The lines with fields "NaN" or "" or "null" are filtered out.
      *
      * @param lines   : List[String]
      * @param columns : ListBuffer[List[String]
      * @return
      */
    @tailrec
    private def findColumnsFromInputLines(lines: Traversable[String], columns: ListBuffer[List[String]]): List[List[String]] =
      lines.isEmpty match {
        case true => columns.reverse.toList
        case false =>
          val currentLine = lines.head
          val tailLines = lines.tail
          val columnsInCurrentLine: Array[String] = currentLine.split("\\t")
          val validLine: Boolean = columnsInCurrentLine.forall { p =>
            p != "NaN" && !p.isEmpty && p != "\"\"" && p != "null"
          }
          validLine match {
            case false => findColumnsFromInputLines(tailLines, columns)
            case true => findColumnsFromInputLines(tailLines, columns += columnsInCurrentLine.toList)
          }
      }
  }
}
