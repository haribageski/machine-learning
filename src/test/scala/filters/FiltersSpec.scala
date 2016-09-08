package filters

import analyzers.SentimentAnalyzer
import filters.DefaultFilterData._
import filters.DefaultFilterParameterGivenYears._
import filters.DefaultFilterParameterGivenDates._
import filters.FilterSyntax.FilterOps
import model.{CombinedCompanyParameters, DateExtended}
import model.DateExtended._
import model.dailyFinancialParameters.{CompanyDailyFinData, CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import model.dailyNewsParameters.{CompanyAllNews, News}
import model.sentiment.CompanyNewsSentiment
import model.yearlyFinancialParameters.{CompanyExtendedFinData, CompanyYearlyFinData, CompanyYearlyFinDataEntry, CompanyYearlyFinParameter}
import org.scalatest.{FlatSpec, Matchers}
import utils.ordered.OrderedSyntax._
import utils.readers.ReadableDefaults.{CombinedCompanyParametersReader, CompanyDailyFinDataReader, CompanyNewsReader, ErrorValidation}
import utils.readers.ReadableParameterDefaults.CompanyDailyFinParameterReader

import scalaz.Scalaz._
import scalaz._
import scala.collection.immutable.TreeSet
import scalaz.Validation

class FiltersSpec extends FlatSpec with Matchers {

  "filter()" should "return parameter that contains only consistent in year entries" in {

    val sym = "EZPW"
    val dividends = List(
      CompanyDailyFinDataEntry(sym, 0.00400000018998981, fromString("07/08/1998")),
      CompanyDailyFinDataEntry(sym, 0.00400000018998981, fromString("20/11/1998")),
      CompanyDailyFinDataEntry(sym, 0.00432999990880489, fromString("04/02/2000"))
    )

    val earliestD = CompanyDailyFinDataEntry(sym, 0.00432999990880489, fromString("04/02/2000"))
    val oldestD = CompanyDailyFinDataEntry(sym, 0.00400000018998981, fromString("07/08/1998"))

    val toComp = CompanyDailyFinParameter(
      sym, Some(oldestD), Some(earliestD), dividends,
      Map(
        1998 -> List(
          0.00400000018998981d,
          0.00400000018998981d
        ),
        2000 -> List(
          0.00432999990880489d
        )
      )
    )
    val dividendsRead: ErrorValidation[CompanyDailyFinParameter] = CompanyDailyFinParameterReader.readDividendFromFile(sym)
    val synchronizedDividends: Validation[String, CompanyDailyFinParameter] = dividendsRead.map(_.filter(Set(1998, 2000)))

    synchronizedDividends.map(_ == toComp should be(true))
  }


  "filter" should "return parameter that contains only consistent in year entries" in {

    val companyYearlyFinParameter1: CompanyYearlyFinParameter = CompanyYearlyFinParameter("A")

    val entry1 = CompanyYearlyFinDataEntry(124.2, 2015)
    val companyYearlyFinParameter2 = companyYearlyFinParameter1.addEntry(entry1)

    val entry2 = CompanyYearlyFinDataEntry(134.2, 2014)
    val companyYearlyFinParameter3 = companyYearlyFinParameter2.addEntry(entry2)

    companyYearlyFinParameter3.filter(Set(2015)) should be(companyYearlyFinParameter2)
    companyYearlyFinParameter3.filter(Set.empty[Int]) should be(companyYearlyFinParameter1)
    companyYearlyFinParameter3.filter(Set(2015, 2014)) should be(companyYearlyFinParameter3)
  }

  "filter" should "return an empty set if rOE has no consistent years" in {

    val companyYearlyFinParameter1: CompanyYearlyFinParameter = CompanyYearlyFinParameter("A")
    val entry1 = CompanyYearlyFinDataEntry(124.2, 2015)
    val companyYearlyFinParameter2 = companyYearlyFinParameter1.addEntry(entry1)

    val companyYearlyFinData =
      CompanyYearlyFinData("A", companyYearlyFinParameter2, companyYearlyFinParameter2, CompanyYearlyFinParameter("A"), companyYearlyFinParameter2)
    val companyDailyFinData = CompanyDailyFinData("A")
    val company = CompanyExtendedFinData(companyYearlyFinData, companyDailyFinData, None, None, None)
    company.filter.map(_.companyDailyFinData should be(CompanyDailyFinData("A")))
    company.filter.map(_.companyYearlyFinData should be(CompanyYearlyFinData("A")))
  }


  "CompanyAllNewsFilter.filter(consistentYears: Set[Int])" should
    "return CompanyAllNewsFilter that contains only consistent in year entries" in {
    val news1 = News("A", fromString("10/03/2015"), 2015,
      "Agilent Technologies Receives $47.28 Consensus Price Target from Brokerages ...",
      "Agilent Technologies Receives $47.28 Consensus Price Target from Brokerages ... WKRB News - Mar 10, 2015 Agilent Technologies logo Shares of Agilent Technologies (NYSE:A) have received an average rating of ?Hold? from the fourteen analysts that are covering the company, American Banking News reports. Nine research analysts have rated the stock with a hold&nbsp;..."
    )
    val news2 = News("A", fromString("10/03/2014"), 2014,
      "First Call Rating Update on Agilent Technologies, Inc.",
      "First Call Rating Update on Agilent Technologies, Inc. Ashburn Daily - Mar 11, 2015 Agilent Technologies, Inc. (NYSE:A) was down 2.66% or 1.11 points for the day. The opening trade was executed at $41.22 and the final trade was executed at $40.63.UBS Rating Disclosure on Agilent Technologies, Inc. - Markets BureauStocks to Watch: Agilent Technologies Inc , Service Corporation International ... - Rock Hill Daily"
    )
    val companyAllNews = CompanyAllNews(
      "A",
      Stream(news1, news2)
    )
    companyAllNews.filter(Set(2014)).news should be(List(news2))
    companyAllNews.filter(Set(2015)).news should be(List(news1))
    companyAllNews.filter(Set.empty[Int]) should be(CompanyAllNews("A", Stream.empty))
    companyAllNews.filter(Set(2014, 2015)) should be(companyAllNews)
  }

  "CompanyNewsSentimentFilter" should
    "return CompanyNewsSentimentFilter consistent with the given dates" in {
    val allCompanyNewsV: ErrorValidation[CompanyAllNews] = CompanyNewsReader.readDataFromFile("Example")

    val sentimentInOneGo: Validation[String, CompanyNewsSentiment] =
    allCompanyNewsV.map(
      allCompanyNews =>
        SentimentAnalyzer.evaluateSentiOfAllCompanyNews(allCompanyNews)
    )

    val filteredSentiment =
      sentimentInOneGo.filter(Set(fromString("18/06/2013")))

    (filteredSentiment |@| sentimentInOneGo) {
      (validat1: CompanyNewsSentiment, validat2: CompanyNewsSentiment) =>
        validat1.avgSentiPerDateDescript should be(
          validat2.avgSentiPerDateDescript.filterKeys(_ == fromString("18/06/2013"))
        )
    }
  }

  "CombinedCompanyParametersFilter.filter(dates)" should
    "return CombinedCompanyParametersFilter that contains only consistent in dates entries" in {
    val symbol = "Example"
    val companyYearlyFinParameter2 = CompanyYearlyFinParameter("Example")
      .addEntry(CompanyYearlyFinDataEntry(124.2, 2015))
      .addEntry(CompanyYearlyFinDataEntry(124.2, 2014))
      .addEntry(CompanyYearlyFinDataEntry(124.2, 2013))
      .addEntry(CompanyYearlyFinDataEntry(124.2, 2012))
    val companyYearlyFinData =
      CompanyYearlyFinData(symbol, companyYearlyFinParameter2, companyYearlyFinParameter2, companyYearlyFinParameter2, companyYearlyFinParameter2)

    val dividends = List(
      CompanyDailyFinDataEntry(symbol, 0.00400000018998981, fromString("07/08/2012")),
      CompanyDailyFinDataEntry(symbol, 0.00400000018998981, fromString("07/08/2013")),
      CompanyDailyFinDataEntry(symbol, 0.00400000018998981, fromString("04/04/2014")),
      CompanyDailyFinDataEntry(symbol, 0.00400000018998981, fromString("20/11/2015")),
      CompanyDailyFinDataEntry(symbol, 0.00432999990880489, fromString("04/02/2015"))
    )
    val earliestD = CompanyDailyFinDataEntry(symbol, 0.00432999990880489, DateExtended.fromString("04/02/2015"))
    val oldestD = CompanyDailyFinDataEntry(symbol, 0.00400000018998981, DateExtended.fromString("07/08/2012"))
    val companyDailyFinParam = CompanyDailyFinParameter(
      symbol, Some(oldestD), Some(earliestD), dividends,
      Map(
        2012 -> List(
          0.00400000018998981d
        ),
        2013 -> List(
          0.00400000018998981d
        ),
        2014 -> List(
          0.00400000018998981d
        ),
        2015 -> List(
          0.00400000018998981d,
          0.00432999990880489d
        )
      )
    )
    val companyDailyFinData = CompanyDailyFinData(symbol, companyDailyFinParam, companyDailyFinParam, companyDailyFinParam)

    val extendedFinData = CompanyExtendedFinData(companyYearlyFinData, companyDailyFinData)
      .deriveAdditionalFinParameters()

    val allCompanyNews: ErrorValidation[CompanyAllNews] = CompanyNewsReader.readDataFromFile(symbol)
    val sentimentInOneGo: Validation[String, CompanyNewsSentiment] = allCompanyNews.map(x => SentimentAnalyzer.evaluateSentiOfAllCompanyNews(x))


    val combinedCompanyParameters: Validation[String, Option[CombinedCompanyParameters]] = sentimentInOneGo.map {
      (senti: CompanyNewsSentiment) => extendedFinData.map(CombinedCompanyParameters(symbol, _, Some(senti)))
    }

    val combinedCompanyParametersFiltered: Validation[String, Option[CombinedCompanyParameters]] =
      combinedCompanyParameters.map(_.flatMap(_.filter))

    (combinedCompanyParametersFiltered |@| allCompanyNews) {
      (param1: Option[CombinedCompanyParameters], param2: CompanyAllNews) => {
        //the filtered extendedFin in CombinedParams should be same as first being filtered itself, and then filtered by news dates
        param1.map{ (combined: CombinedCompanyParameters) =>
          combined.extendedFinData.companyDailyFinData should be {
            extendedFinData.flatMap(_.filter).map(_.companyDailyFinData.filter(param2.news.map(_.dateOfNews).toSet)).get
          }
          combined.extendedFinData.companyYearlyFinData should be {
            extendedFinData.flatMap(_.filter).map(_.companyYearlyFinData.filter(param2.news.map(_.dateOfNews.getYear).toSet)).get
          }
        }
        //the filtered newsSentiment in CombinedParams should be same as newsSentiment being filtered by parameterDividends dates
        sentimentInOneGo.map(news => {
          param1.map(combined => combined.newsSentiment should be {
            Some(news.filter(combined.extendedFinData.companyDailyFinData.parameterSUEs.allCompanyEntriesOfOneDailyParam.map(_.date).toSet))
          })
        })
      }
    }
  }

  "CombinedCompanyParametersFilter.filter()" should
    "return CombinedCompanyParametersFilter that contains only consistent in year entries" in {
    val symbol = "Example2"
    val combinedNonFiltered = CombinedCompanyParametersReader.readDataFromFile(symbol)
    val combinedNonFilteredWithDerivedParams = combinedNonFiltered.map { notFiltered => notFiltered.copy(extendedFinData =
      notFiltered.extendedFinData.deriveAdditionalFinParameters.get)
    }
    val filteredCombinedParams = combinedNonFiltered.map(_.filter)

//    (filteredCombinedParams |@| CompanyDailyFinDataReader.readDataFromFile(symbol)) { (param1, param2) =>
//      param1.extendedFinData.companyDailyFinData.parameterDividends shouldBe {
//        param2.filter(Set(fromString("04/04/2014"))).parameterDividends
//      }
//    }
    (filteredCombinedParams |@| CompanyDailyFinDataReader.readDataFromFile(symbol)) { (param1, param2) =>
      param1.map(_.extendedFinData.companyDailyFinData.parameterSUEs shouldBe {
        param2.filter(Set(fromString("04/04/2014"))).parameterSUEs
      })
    }
    pending
    (filteredCombinedParams |@| CompanyDailyFinDataReader.readDataFromFile(symbol)) { (param1, param2: CompanyDailyFinData) =>
      param1.map(_.extendedFinData.companyDailyFinData.parameterQuotes shouldBe
        param2.filter(Set(fromString("04/04/2014"))).parameterQuotes
      )}
    (filteredCombinedParams |@| combinedNonFilteredWithDerivedParams) { (param1, param2) =>
      param1.map(_.extendedFinData.companyYearlyFinData shouldBe
        param2.extendedFinData.companyYearlyFinData
          .filter(Set(2014))
      )}
    (filteredCombinedParams |@| combinedNonFilteredWithDerivedParams) { (param1, param2) =>
      param1.map(_.newsSentiment shouldBe
        param2.newsSentiment.map {
          _.filter(Set(fromString("04/04/2014")))
        })
    }
    (filteredCombinedParams |@| combinedNonFilteredWithDerivedParams) { (param1, param2) =>
      param1.map(_.extendedFinData.companyBMratio shouldBe
        param2.extendedFinData.companyBMratio
          .map(_.filter(Set(2014)))
      )}
    (filteredCombinedParams |@| combinedNonFilteredWithDerivedParams) { (param1, param2) =>
      param1.map(_.extendedFinData.companySize shouldBe
        param2.extendedFinData.companySize
          .map(_.filter(Set(2014)))
      )}
    (filteredCombinedParams |@| combinedNonFilteredWithDerivedParams) { (param1, param2) =>
      param1.map(_.extendedFinData.companyMarketValues shouldBe
        param2.extendedFinData.companyMarketValues
          .map(_.filter(Set(2014)))
      )}
  }
}
