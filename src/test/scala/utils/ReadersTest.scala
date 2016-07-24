package utils

import dailyFinancialParameters.{CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import dailyNewsParameter.{CompanyAllNews, News}
import org.scalatest.{FlatSpec, Matchers}
import utils.ordered.OrderedSyntax._
import utils.readers.ReadableColumnsDefaults.ColumnsReader
import utils.readers.ReadableDefaults.CompanyNewsReader
import utils.readers.ReadableParameterDefaults.CompanyDailyFinParameterReader

import scala.collection.immutable.TreeSet

class ReadersTest  extends FlatSpec with Matchers {
  "readColumn()" should "return the columns as List[List[String]], the first list being the list of all lines" in {
    val filePath = "resources/dividends/NOOF.txt"
    val filePath2 = "resources/dividends/wrongFormat.txt"

    ColumnsReader.readColumnsFromFile(filePath) should be(
      List(
        List("NOOF", "19/03/2008", "0.125"), List("NOOF", "19/12/2007", "0.125"),
        List("NOOF", "13/09/2007", "0.125"), List("NOOF", "31/05/2007", "0.125")
      )
    )
    ColumnsReader.readColumnsFromFile(filePath2) should be(
      List(
        List("A", "27/03/2015", "0.1")
      )
    )
    //    ColumnsReader.readColumnsFromFile(filePath2 + "2") should be (
    //      List(
    //        List("A", "27/03/2015", "0.1")
    //      )
    //    )
    //    ColumnsReader.readColumnsFromFile(filePath2 + "3") should be (
    //      List(
    //        List("A", "27/03/2015", "0.1")
    //      )
    //    )
  }

  "readCompanyDividends() " should "read all CompanyDailyFinParameter dividends of the company from file" in {
    import utils.readers.ReadableDefaults._
    val sym = "NOOF"
    val companyDividends = CompanyDailyFinParameter(sym)

    val dividends = List(
      CompanyDailyFinDataEntry("NOOF", 0.125, DateExtended("31/05/2007")),
      CompanyDailyFinDataEntry("NOOF", 0.125, DateExtended("13/09/2007")),
      CompanyDailyFinDataEntry("NOOF", 0.125, DateExtended("19/12/2007")),
      CompanyDailyFinDataEntry("NOOF", 0.125, DateExtended("19/03/2008"))
    )

    val earliestD = CompanyDailyFinDataEntry("NOOF", 0.125, DateExtended("19/03/2008"))
    val oldestD = CompanyDailyFinDataEntry("NOOF", 0.125, DateExtended("31/05/2007"))

    val dividendsRead: CompanyDailyFinParameter = CompanyDailyFinParameterReader.readDividendFromFile("NOOF")
    val toComp = CompanyDailyFinParameter(sym, Some(oldestD), Some(earliestD), dividends,
      Map(2007 -> TreeSet(
        CompanyDailyFinDataEntry("NOOF", 0.125, DateExtended("31/05/2007")),
        CompanyDailyFinDataEntry("NOOF", 0.125, DateExtended("13/09/2007")),
        CompanyDailyFinDataEntry("NOOF", 0.125, DateExtended("19/12/2007"))
      ),
        2008 -> TreeSet(
          CompanyDailyFinDataEntry("NOOF", 0.125, DateExtended("19/03/2008"))
        )
      )
    )
    dividendsRead == toComp should be(true)
  }


  "readCompanySUEs() " should "read all CompanyDailyFinParameter expected earnings of the company from file" in {
    val sym = "NOOF"
    val companySUE = CompanyDailyFinParameter(sym, null, null, List.empty[CompanyDailyFinDataEntry],
      Map.empty[Int, TreeSet[CompanyDailyFinDataEntry]])

    val sues = List(
      CompanyDailyFinDataEntry("NOOF", 85.7099990844727, DateExtended("10/06/2010")),
      CompanyDailyFinDataEntry("NOOF", -57.1399993896484, DateExtended("06/08/2010")),
      CompanyDailyFinDataEntry("NOOF", -133.330001831055, DateExtended("05/11/2010")),
      CompanyDailyFinDataEntry("NOOF", -80, DateExtended("04/02/2011"))
    )

    val earliestS = CompanyDailyFinDataEntry("NOOF", -80, DateExtended("04/02/2011"))
    val oldestS = CompanyDailyFinDataEntry("NOOF", 85.7099990844727, DateExtended("10/06/2010"))

    val suesRead = CompanyDailyFinParameterReader.readEarningSurpriseFromFile("NOOF")
    val toComp = CompanyDailyFinParameter(sym, Some(oldestS), Some(earliestS), sues,
      Map(2010 -> TreeSet(
        CompanyDailyFinDataEntry("NOOF", 85.7099990844727, DateExtended("10/06/2010")),
        CompanyDailyFinDataEntry("NOOF", -57.1399993896484, DateExtended("06/08/2010")),
        CompanyDailyFinDataEntry("NOOF", -133.330001831055, DateExtended("05/11/2010"))
      ),
        2011 -> TreeSet(
          CompanyDailyFinDataEntry("NOOF", -80, DateExtended("04/02/2011"))
        ))
    )
    suesRead == toComp should be(true)
  }



  "readCompanyQuotes() " should "read all CompanyDailyFinParameter quotes in expected format" in {
    val sym = "test"
    val companyQuotes = CompanyDailyFinParameter(sym, null, null, List.empty[CompanyDailyFinDataEntry],
      Map.empty[Int, TreeSet[CompanyDailyFinDataEntry]])

    val quotes = List(
      CompanyDailyFinDataEntry("test", 2.01, DateExtended("22/11/2012")),
      CompanyDailyFinDataEntry("test", 1.99, DateExtended("23/11/2012")),
      CompanyDailyFinDataEntry("test", 2.01, DateExtended("26/11/2012")),
      CompanyDailyFinDataEntry("test", 2.02, DateExtended("27/11/2012")),
      CompanyDailyFinDataEntry("test", 2.02, DateExtended("28/11/2012"))
    )

    val oldestQ = CompanyDailyFinDataEntry("test", 2.01, DateExtended("22/11/2012"))
    val earliestQ = CompanyDailyFinDataEntry("test", 2.02, DateExtended("28/11/2012"))

    val quotesRead: CompanyDailyFinParameter = CompanyDailyFinParameterReader.readQuotesFromFile("test")

    val toComp = CompanyDailyFinParameter(sym, Some(oldestQ), Some(earliestQ), quotes,
      Map(2012 -> TreeSet(
        CompanyDailyFinDataEntry("test", 2.01, DateExtended("22/11/2012")),
        CompanyDailyFinDataEntry("test", 1.99, DateExtended("23/11/2012")),
        CompanyDailyFinDataEntry("test", 2.01, DateExtended("26/11/2012")),
        CompanyDailyFinDataEntry("test", 2.02, DateExtended("27/11/2012")),
        CompanyDailyFinDataEntry("test", 2.02, DateExtended("28/11/2012"))
      ))
    )
    quotesRead == toComp should be(true)
  }


  "CompanyNewsReader.readDataFromFile() " should "read all Company News in expected format" in {
    val sym = "Example"
    val newsRead = CompanyNewsReader.readDataFromFile(sym)
    newsRead.news(2) should be(
      News(sym, DateExtended("18/06/2013"), 2013, "Agilent Technologies Inc Announces Offering of Senior Notes",
        "Agilent Technologies Inc Announces Offering of Senior Notes Reuters Key Development - Jun 18, 2013")
    )

    newsRead.news(0) should be(
      News(sym, DateExtended("02/03/2014"), 2014, "Update: Agilent Technologies, Inc. Short Interest Grows by 5.8%",
        "Update: Agilent Technologies, Inc. Short Interest Grows by 5.8% Wall Street Pulse - Mar 2, 2015 Agilent Technologies, Inc. (NYSE:A) reported a rise of 132,877 shares or 5.8% in the short interest. The remaining shorts are 0.7% of the total floated shares.Share Price of Agilent Technologies, Inc. Rally 0.62% - Ashburn DailyAgilent Technologies Receives &quot;A-&quot; Credit Rating from Morningstar (A) - sleekmoney")
    )
    newsRead.news(1) should be(
      News(sym, DateExtended("18/06/2013"), 2013, "Agilent Technologies Inc Prices $600 Million of Senior Notes",
        "Agilent Technologies Inc Prices $600 Million of Senior Notes Reuters Key Development - Jun 18, 2013")
    )
  }
}
