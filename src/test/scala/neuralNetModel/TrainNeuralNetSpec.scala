package neuralNetModel

import analyzers.SentimentAnalyzer
import breeze.linalg.DenseMatrix
import org.scalatest.{FlatSpec, Matchers}
import utils.readers.ReadableDefaults._
import filters.DefaultFilterData._
import filters.FilterSyntax.FilterOps
import model.dailyNewsParameters.{CompanyAllNews, News}
import model.CombinedCompanyParameters
import model.sentiment.CompanyNewsSentiment
import neuralNetModel.TrainingMatrixBuilder._
import org.joda.time.DateTime
import org.scalatest.concurrent.ScalaFutures

import scala.collection.immutable.HashSet
import scalaz._
import scalaz.Scalaz._


class TrainNeuralNetSpec   extends FlatSpec with Matchers with ScalaFutures {
  val processors: Int = Runtime.getRuntime.availableProcessors

  "createMatrix()" should "return a tuple of set of rows, each row being a list, and set of results - Quote values" in {

    val symbols = CombinedCompanyParametersReader.getNamesOfFiles().toSeq
    println("HEAD:" + symbols.head)
    println("symbols size in CombinedCompanyParametersReader:" + CombinedCompanyParametersReader.getNamesOfFiles().size)

    lazy val allCompaniesParams: Vector[Validation[String, CombinedCompanyParameters]] = symbols.take(200).map {
      fileName =>
        println("file to read: " + fileName)
        val combinedNonFiltered: Validation[String, CombinedCompanyParameters] = CombinedCompanyParametersReader.readDataFromFile(fileName)
        val combinedNonFilteredWithDerivedParams = combinedNonFiltered.map { notFiltered => notFiltered.copy(extendedFinData =
          notFiltered.extendedFinData.deriveAdditionalFinParameters.get)
        }
        combinedNonFilteredWithDerivedParams
    }.toVector

    val allNews: Vector[ErrorValidation[CompanyAllNews]] =
      symbols.take(200).map {
        fileName => CompanyNewsReader.readDataFromFile(fileName)
      }.toVector

    val allCombinedValidations: Vector[Validation[String, CombinedCompanyParameters]] = {
      allCompaniesParams.zip(allNews)
        .grouped((allCompaniesParams.size.toDouble / processors).toInt + 1).toSeq.par //group of jobs to be processed in parallel
        .map {
        group => group.map {
          validatAllParamsWithNews =>
            (validatAllParamsWithNews._1 |@| validatAllParamsWithNews._2) {
              (allParams: CombinedCompanyParameters, news: CompanyAllNews) =>
                val emptyCompanyNewsSentiWithDates = CompanyNewsSentiment(news.symbol, news.news.map(_.dateOfNews).toSet) //we only store the dates of the news so we can filter them
                val allParamsFilteredNoSentiment: Option[CombinedCompanyParameters] = allParams.copy(newsSentiment = Some(emptyCompanyNewsSentiWithDates))
                  .filter
                val newsFiltered: Stream[News] = news.news.filter(news =>
                  allParamsFilteredNoSentiment.map(_.newsSentiment.get.dates.contains(news.dateOfNews)).getOrElse(false)
                )
                val senti: CompanyNewsSentiment = SentimentAnalyzer.evaluateSentiOfAllCompanyNews(news.copy(news = newsFiltered))
                allParamsFilteredNoSentiment.map(_.copy(newsSentiment = Some(senti))).getOrElse(CombinedCompanyParameters(news.symbol, allParams.extendedFinData, None))
            }
        }
      }.fold(Vector.empty[Validation[String, CombinedCompanyParameters]])((vector1, vector2) => vector1 ++ vector2)
    }


    val allCombined: Vector[CombinedCompanyParameters] = {
      allCombinedValidations.par.aggregate(Vector.empty[CombinedCompanyParameters])((acc, validate) => {
        validate match {
          case Success(a: CombinedCompanyParameters) => a +: acc
          case Failure(e) =>
            println(e)
            acc
        }
      }, (vector1, vector2) => vector1 ++ vector2)
    }


    allCombined.foreach {
      combined =>
        combined.newsSentiment.map { senti =>
          val dates = senti.dates
          //        require(combined.extendedFinData.companyDailyFinData.parameterSUEs.allCompanyEntriesOfOneDailyParam.map(_.date).toSet.diff(dates).isEmpty)
          require(combined.extendedFinData.companySize.get.allCompanyEntriesOfOneYearlyParam.map(_.year).toSet.diff(dates.map(_.getYear)).isEmpty)
          require(combined.extendedFinData.companyBMratio.get.allCompanyEntriesOfOneYearlyParam.map(_.year).toSet.diff(dates.map(_.getYear)).isEmpty)
          require(combined.extendedFinData.companyMarketValues.get.allCompanyEntriesOfOneYearlyParam.map(_.year).toSet.diff(dates.map(_.getYear)).isEmpty)
          require(combined.extendedFinData.companyYearlyFinData.bookValue.allCompanyEntriesOfOneYearlyParam.map(_.year).toSet.diff(dates.map(_.getYear)).isEmpty)
          require(combined.extendedFinData.companyYearlyFinData.rOE.allCompanyEntriesOfOneYearlyParam.map(_.year).toSet.diff(dates.map(_.getYear)).isEmpty)
          require(combined.extendedFinData.companyYearlyFinData.accrual.allCompanyEntriesOfOneYearlyParam.map(_.year).toSet.diff(dates.map(_.getYear)).isEmpty)
          require(combined.extendedFinData.companyYearlyFinData.shares.allCompanyEntriesOfOneYearlyParam.map(_.year).toSet.diff(dates.map(_.getYear)).isEmpty)
          require(combined.newsSentiment.get.dates.diff(dates).isEmpty)
        }
    }

    createMatrix(allCombined.toList)._1.foreach(x => println("create cols:" + x.size))
    val trainingCrossValTestingMatrix: (Set[List[Double]], Set[List[Double]], Set[List[Double]], Set[Double], Set[Double], Set[Double]) =
      TrainingMatrixBuilder.divideToTrainingCrossValidationTest(createMatrix(allCombined.toList))
    val trainingMatrix = (trainingCrossValTestingMatrix._1, trainingCrossValTestingMatrix._4)

//    trainingMatrix._1.foreach(x => println("trainingMatrix cols:" + x.size))
    println("trainingMatrix rows:" + trainingMatrix._1.size)
    val representationNeuralNet: RepresentationNeuralNet = RepresentationNeuralNet((trainingMatrix._1 zip trainingMatrix._2).toArray)   //TODO: FIx this
    println("representationNeuralNet cols:" + representationNeuralNet.Xs.cols)
    val derivedTheta: List[DenseMatrix[Double]] = TrainNeuralNet(representationNeuralNet).learnTheta()
    println("theta1:" + derivedTheta.head)
    println("theta2:" + derivedTheta.tail.head)
    println("theta3:" + derivedTheta.tail.head)

    println(s"trainingCrossValTestingMatrix sizes: ${trainingCrossValTestingMatrix._1.size} x ${trainingCrossValTestingMatrix._1.head.size}")
    println(s"trainingCrossValTestingMatrix sizes: ${trainingCrossValTestingMatrix._4.size} x 1")
    val testingMatrix = (trainingCrossValTestingMatrix._3, trainingCrossValTestingMatrix._6)
    val testingNeuralNet = RepresentationNeuralNet((testingMatrix._1 zip testingMatrix._2).toArray)

    val testSetError = TrainNeuralNet(testingNeuralNet).testSetError(testingNeuralNet, derivedTheta)
    println("testSetError:" + testSetError)
    1 should be(1)
  }

}
