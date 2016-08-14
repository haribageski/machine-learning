package neuralNetModel

import analyzers.SentimentAnalyzer
import org.scalatest.{FlatSpec, Matchers}
import utils.readers.ReadableDefaults._
import filters.DefaultFilterData._
import filters.FilterSyntax.FilterOps
import model.dailyNewsParameters.CompanyAllNews
import model.CombinedCompanyParameters
import neuralNetModel.TrainingMatrixBuilder._
import scalaz._
import scalaz.Scalaz._

class TrainingMatrixBuilderSpec  extends FlatSpec with Matchers {
  val processors: Int =  Runtime.getRuntime.availableProcessors

  "createMatrix()" should "return a tuple of set of rows, each row being a list, and set of results - Quote values" in {
    val symbol = "Example"
    val combinedNonFilteredNoNewsData: Validation[String, CombinedCompanyParameters] =
      CombinedCompanyParametersReader.readDataFromFile(symbol)
    //    val combinedNonFiltered = combinedNonFilteredNoNewsData.map(combinedParams => {
    //      combinedParams.copy(allNews = Some(SentimentAnalyzer.evaluateSentiOfAllCompanyNews()))
    //    })
    val combinedFilteredWithDerivedParams: Validation[String, CombinedCompanyParameters] = combinedNonFilteredNoNewsData.map(_.filter)

    val matrix: Validation[String, (Set[List[Double]], Set[Double])] = combinedFilteredWithDerivedParams.map(x => createMatrix(List(x)))
    1 should be(1)




    val symbols = CombinedCompanyParametersReader.getNamesOfFiles().toSeq
    println("HEAD:" + symbols.head)
    println("symbols size in CombinedCompanyParametersReader:" + CombinedCompanyParametersReader.getNamesOfFiles().size)

    lazy val allCompaniesParams: Seq[Validation[String, CombinedCompanyParameters]] = symbols.take(1).map {
      fileName =>
        println("file to read: " + fileName)
        CombinedCompanyParametersReader.readDataFromFile(fileName)
    }

    val allNews: Seq[Validation[String, CompanyAllNews]] =
      symbols.take(1).map {
        fileName => CompanyNewsReader.readDataFromFile(fileName)
      }

    val allCombinedSeqOfValidat: Seq[Validation[String, CombinedCompanyParameters]] = {
      allCompaniesParams.zip(allNews)
        .grouped((allCompaniesParams.size.toDouble / processors).toInt + 1).toSeq //group of jobs to be processed in parallel
        .map {
        x => x.map {
          validatAllParamsWithNews =>
            (validatAllParamsWithNews._1 |@| validatAllParamsWithNews._2) {
              (allParams, news) =>
                val senti = SentimentAnalyzer.evaluateSentiOfAllCompanyNews(news)
                allParams.copy(newsSentiment = Some(senti))
            }
        }
      }.fold(Seq.empty)((seq1, seq2) => seq1 ++ seq2)
    }

    val allCombinedSeq: Seq[CombinedCompanyParameters] =
      allCombinedSeqOfValidat.foldLeft(Seq.empty[CombinedCompanyParameters])((acc, validat) => {
        validat match {
          case Success(a: CombinedCompanyParameters) => a +: acc
          case Failure(e) =>
            println(e)
            acc
        }
      })


    println("AllCompaniesSize:" + allCombinedSeq.size)
    val x = createMatrix(allCombinedSeq.toList)
    1 should be(1)
    println("X size:" + x._1.size)
  }
}
