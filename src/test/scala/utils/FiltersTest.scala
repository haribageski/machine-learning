package utils

import model.DateExtended
import model.dailyFinancialParameters.{CompanyDailyFinData, CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import model.yearlyFinancialParameters.{CompanyYearlyExtendedFinData, CompanyYearlyFinData, CompanyYearlyFinDataEntry, CompanyYearlyFinParameter}
import org.scalatest.{FlatSpec, Matchers}
import utils.ordered.OrderedSyntax._
import filters.DefaultFilters._
import filters.FilterSyntax.FilterOps
import model.dailyNewsParameters.{CompanyAllNews, News}
import utils.readers.ReadableParameterDefaults.CompanyDailyFinParameterReader

import scala.collection.immutable.TreeSet

class FiltersTest extends FlatSpec with Matchers {
  "filter()" should "return parameter that contains only consistent in year entries" in {

    val sym = "EZPW"
    val dividends = List(
      CompanyDailyFinDataEntry(sym, 0.00400000018998981, DateExtended("07/08/1998")),
      CompanyDailyFinDataEntry(sym, 0.00400000018998981, DateExtended("20/11/1998")),
      CompanyDailyFinDataEntry(sym, 0.00432999990880489, DateExtended("04/02/2000"))
    )

    val earliestD = CompanyDailyFinDataEntry(sym, 0.00432999990880489, DateExtended("04/02/2000"))
    val oldestD = CompanyDailyFinDataEntry(sym, 0.00400000018998981, DateExtended("07/08/1998"))

    val toComp = CompanyDailyFinParameter(
      sym, Some(oldestD), Some(earliestD), dividends,
      Map(
        1998 -> TreeSet(
          CompanyDailyFinDataEntry(sym, 0.00400000018998981, DateExtended("07/08/1998")),
          CompanyDailyFinDataEntry(sym, 0.00400000018998981, DateExtended("20/11/1998"))
        ),
        2000 -> TreeSet(
          CompanyDailyFinDataEntry(sym, 0.00432999990880489, DateExtended("04/02/2000"))
        )
      )
    )
    val dividendsRead: CompanyDailyFinParameter = CompanyDailyFinParameterReader.readDividendFromFile(sym)
    val synchronizedDividends: CompanyDailyFinParameter = dividendsRead.filter(Set(1998, 2000))

    println("synchronizedDividends:" + synchronizedDividends)
    println("toComp:" + toComp)
    synchronizedDividends == toComp should be(true)
  }


  "filter" should "return parameter that contains only consistent in year entries" in {

    val companyYearlyFinParameter1: CompanyYearlyFinParameter = CompanyYearlyFinParameter("A")

    val entry1 = CompanyYearlyFinDataEntry("A", 124.2, 2015)
    val companyYearlyFinParameter2 = companyYearlyFinParameter1.addEntry(entry1)

    val entry2 = CompanyYearlyFinDataEntry("A", 134.2, 2014)
    val companyYearlyFinParameter3 = companyYearlyFinParameter2.addEntry(entry2)

    companyYearlyFinParameter3.filter(Set(2015)) should be (companyYearlyFinParameter2)
    companyYearlyFinParameter3.filter(Set.empty[Int]) should be(companyYearlyFinParameter1)
    companyYearlyFinParameter3.filter(Set(2015, 2014)) should be(companyYearlyFinParameter3)
  }

  "filter" should "return an empty set if rOE has no consistent years" in {

    val companyYearlyFinParameter1: CompanyYearlyFinParameter = CompanyYearlyFinParameter("A")
    val entry1 = CompanyYearlyFinDataEntry("A", 124.2, 2015)
    val companyYearlyFinParameter2 = companyYearlyFinParameter1.addEntry(entry1)

    val companyYearlyFinData =
      CompanyYearlyFinData("A", companyYearlyFinParameter2, companyYearlyFinParameter2, CompanyYearlyFinParameter("A"), companyYearlyFinParameter2)
    val companyDailyFinData = CompanyDailyFinData("A")
    val company = CompanyYearlyExtendedFinData(companyYearlyFinData, companyDailyFinData, None, None, None)
    company.filter should be (company.copy(CompanyYearlyFinData("A"), CompanyDailyFinData("A")))
  }


  "CompanyAllNewsFilter.filter(consistentYears: Set[Int])" should
    "return CompanyAllNewsFilter that contains only consistent in year entries" in {
    val news1 = News("A", DateExtended("10/03/2015"), 2015,
      "Agilent Technologies Receives $47.28 Consensus Price Target from Brokerages ...",
      "Agilent Technologies Receives $47.28 Consensus Price Target from Brokerages ... WKRB News - Mar 10, 2015 Agilent Technologies logo Shares of Agilent Technologies (NYSE:A) have received an average rating of ?Hold? from the fourteen analysts that are covering the company, American Banking News reports. Nine research analysts have rated the stock with a hold&nbsp;..."
    )
    val news2 = News("A", DateExtended("10/03/2014"), 2014,
      "First Call Rating Update on Agilent Technologies, Inc.",
      "First Call Rating Update on Agilent Technologies, Inc. Ashburn Daily - Mar 11, 2015 Agilent Technologies, Inc. (NYSE:A) was down 2.66% or 1.11 points for the day. The opening trade was executed at $41.22 and the final trade was executed at $40.63.UBS Rating Disclosure on Agilent Technologies, Inc. - Markets BureauStocks to Watch: Agilent Technologies Inc , Service Corporation International ... - Rock Hill Daily"
    )
    val companyAllNews = CompanyAllNews(
      "A",
      List(news1, news2)
    )
    companyAllNews.filter(Set(2014)).news should be (List(news2))
    companyAllNews.filter(Set(2015)).news should be (List(news1))
    companyAllNews.filter(Set.empty[Int]) should be (CompanyAllNews("A", Nil))
    companyAllNews.filter(Set(2014, 2015)) should be (companyAllNews)
  }
}
