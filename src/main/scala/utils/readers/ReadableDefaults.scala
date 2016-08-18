package utils.readers

import java.nio.file.InvalidPathException
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
import better.files.File
import cats.data.Xor.Right
import scalaz.Scalaz._
import scalaz._
import scala.util.{Failure, Success, Try}
import scalaz.{Success => SuccessZ}
import scalaz.Applicative
import scalaz.Validation._


object ReadableDefaults {
  type ErrorValidation[A] = Validation[String, A]

  implicit object CompanyDailyFinDataReader extends Readable[CompanyDailyFinData] {
    type Result = ErrorValidation[CompanyDailyFinData]
    /**
      * Provided a symbol, uses CompanyDailyFinParameterReader.readParameterFromFile() to read
      * the dividends/quotes/SUEs and return new CompanyDailyFinData with all the read entries.
      */
    override def readDataFromFile(symbol: String): ErrorValidation[CompanyDailyFinData] = {

      val companyDailyFinParameter = CompanyDailyFinParameter(symbol)

      val companyDivid: Validation[String, CompanyDailyFinParameter] = CompanyDailyFinParameterReader.readParameterFromFile(
        "resources/dividends/" + symbol + ".txt",
        symbol,
        indexOfValue = 2
      )

      val companyQuotes: Validation[String, CompanyDailyFinParameter] = CompanyDailyFinParameterReader.readParameterFromFile(
          "resources/quotes-prices/" + symbol + ".txt",
          symbol,
          indexOfValue = 3
        )

      val companySUE: Validation[String, CompanyDailyFinParameter] = CompanyDailyFinParameterReader.readParameterFromFile(
          "resources/earning-surprises/" + symbol + ".txt",
          symbol,
          indexOfValue = 2
        )

      (companyDivid |@| companyQuotes |@| companySUE){
        (param1, param2, param3) => CompanyDailyFinData(symbol, param1, param2, param3)
      }
    }

    def symbolsInParam(dirPath:String): Set[String] = Try(File(dirPath)) match {
      case Success(dir) =>
        dir match {
          case File.Type.Directory(files) =>
            dir.list.toSet[File].map(_.name.dropRight(4))
          case _ =>
            println("Not a directory, but a file")
            Set.empty[String]
        }
      case Failure(e) =>
        println("No such file:" + e)
        Set.empty[String]
    }

    override def getNamesOfFiles(): Set[String] = {
      symbolsInParam("resources/dividends/")
        .intersect(symbolsInParam("resources/quotes-prices/"))
        .intersect(symbolsInParam("resources/earning-surprises/"))
    }
  }

  /**
    * The files to read the yearly parameters from are with lines of the following structure:
    * symbol /t date /t parameter1 /t parameter2 /t parameter3 /t parameter4
    */
  implicit object CompanyYearlyFinDataReader extends Readable[CompanyYearlyFinData] {
    val dirPath: String = "resources/yearly-fin-parameters/"
    override def readDataFromFile(symbol: String): ErrorValidation[CompanyYearlyFinData] = {

      //TODO: Get filePath in other way (from config)
      val filePath = "resources/yearly-fin-parameters/" + symbol + ".txt"

      val indexesOfValues = Seq(2, 3, 4, 5)
      val inputDataFromFileO: Validation[String, List[List[String]]] = ColumnsReader.readColumnsFromFile(filePath)

      val filteredInput: Validation[String, List[List[String]]] =
        inputDataFromFileO.map(_.filter(Validator.validateValueInLines(indexesOfValues)))

      val filteredZeroShares = filteredInput.map(_.filter(line => line(3).toInt != 0))

      //TODO: The followint mappings can be combined and done in one iteration
      lazy val entriesForParameterBookVal: Validation[String, List[CompanyYearlyFinDataEntry]] =
        filteredZeroShares.map(
          _.map(
            line => CompanyYearlyFinDataEntry(
              symbol, line(2).toDouble, DateExtended(line(1)).dateExtended.getYear
            )
          ))
      lazy val entriesForParameterShares: Validation[String, List[CompanyYearlyFinDataEntry]] =
        filteredZeroShares.map(
          _.map(
            line => CompanyYearlyFinDataEntry(
              symbol, line(3).toDouble, DateExtended(line(1)).dateExtended.getYear
            )
          ))
      lazy val entriesForParameterROEs: Validation[String, List[CompanyYearlyFinDataEntry]] =
        filteredZeroShares.map(
          _.map(
            line => CompanyYearlyFinDataEntry(
              symbol, line(4).toDouble, DateExtended(line(1)).dateExtended.getYear
            )
          ))
      lazy val entriesForParameterAccruals: Validation[String, List[CompanyYearlyFinDataEntry]] =
        filteredZeroShares.map(
          _.map(
            line => CompanyYearlyFinDataEntry(
              symbol, line(5).toDouble, DateExtended(line(1)).dateExtended.getYear
            ))
        )

      val emptyBookValue = CompanyYearlyFinParameter(symbol)
      val emptyShares = CompanyYearlyFinParameter(symbol)
      val emptyROE = CompanyYearlyFinParameter(symbol)
      val emptyAccrual = CompanyYearlyFinParameter(symbol)

      lazy val allBookValues: ErrorValidation[CompanyYearlyFinParameter] =
        entriesForParameterBookVal.map(_.foldLeft(emptyBookValue)((acc, entryYoAdd) => acc.addEntry(entryYoAdd)))

      lazy val allShares: ErrorValidation[CompanyYearlyFinParameter] =
        entriesForParameterShares.map(_.foldLeft(emptyShares)((acc, entryToAdd) => acc.addEntry(entryToAdd)))

      lazy val allROEs: ErrorValidation[CompanyYearlyFinParameter] =
        entriesForParameterROEs.map(_.foldLeft(emptyROE)((acc, entryYoAdd) => acc.addEntry(entryYoAdd)))

      lazy val allAccruals: ErrorValidation[CompanyYearlyFinParameter] =
        entriesForParameterAccruals.map(_.foldLeft(emptyAccrual)((acc, entryYoAdd) => acc.addEntry(entryYoAdd)))

      (allBookValues |@| allShares |@| allROEs |@| allAccruals) {
        (param1, param2, param3, param4) => {
          CompanyYearlyFinData(
            symbol,
            param1,
            param2,
            param3,
            param4
          )
        }
      }
    }

    override def getNamesOfFiles(): Set[String] = {
      Try(File(dirPath)) match {
        case Success(dir) =>
          dir match {
            case File.Type.Directory(files) =>
              dir.list.toSet[File].map(_.name.dropRight(4))
            case _ =>
              println("Not a directory, but a file")
              Set.empty[String]
          }
        case Failure(e) =>
          println("No such file:" + e)
          Set.empty[String]
      }
    }
  }

  implicit object CompanyNewsReader extends Readable[CompanyAllNews] {
    val dirPath = "resources/news/"

    override def readDataFromFile(symbol: String): ErrorValidation[CompanyAllNews] = {
      //TODO: Get filePath in other way (from config)
      val filePath = dirPath + "google_news_" + symbol + ".txt"

      val inputDataFromFileE: Validation[String, List[List[String]]] = ColumnsReader.readColumnsFromFile(filePath)
      val allCompanyNewsE: Validation[String, Stream[News]] = inputDataFromFileE.map(_.map { line =>
        val date = DateExtended(line(1))
        News(symbol, date.dateExtended, date.dateExtended.getYear, line(2), line(3))
      }.toStream)
      allCompanyNewsE.map(CompanyAllNews(symbol, _))
    }

    override def getNamesOfFiles(): Set[String] = {
      Try(File(dirPath)) match {
        case Success(dir) =>
          dir match {
            case File.Type.Directory(files) =>
              dir.list.toSet[File].map(_.name)
            case _ =>
              println("Not a directory, but a file")
              Set.empty[String]
          }
        case Failure(e) =>
          println("No such file:" + e)
          Set.empty[String]
      }
    }

    def getFileNameWithoutPrefixNorSufix(): Set[String] = {
      getNamesOfFiles.map(_.drop(12).dropRight(4))   //it drops the prefix google_news_
    }
  }

  implicit object CombinedCompanyParametersReader extends Readable[CombinedCompanyParameters] {
    override def readDataFromFile(sym: String): Validation[String, CombinedCompanyParameters] = {
//      lazy val allCompanyNews = CompanyNewsReader.readDataFromFile(sym)
      lazy val companyYearly = CompanyYearlyFinDataReader.readDataFromFile(sym)
      lazy val companyDaily = CompanyDailyFinDataReader.readDataFromFile(sym)

      (companyYearly |@| companyDaily) {
        (param1, param2) =>
        CombinedCompanyParameters(
          sym,
          CompanyExtendedFinData(
            param1,
            param2
          ),
          None
        )
      }
    }

    override def getNamesOfFiles(): Set[String] = {
      CompanyNewsReader.getFileNameWithoutPrefixNorSufix()
        .intersect(CompanyYearlyFinDataReader.getNamesOfFiles())
        .intersect(CompanyDailyFinDataReader.getNamesOfFiles())
    }
  }
}
