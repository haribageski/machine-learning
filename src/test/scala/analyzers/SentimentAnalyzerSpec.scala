package analyzers

import model.DateExtended
import model.dailyNewsParameters.{CompanyAllNews, News}
import model.sentiment.CompanyNewsSentiment
import org.scalatest.{FlatSpec, Matchers}

class SentimentAnalyzerSpec  extends FlatSpec with Matchers {
  "evaluateSentiOfAllCompanyNews()" should
    "return cumulative Sentiment" in {
    val symbol: String = "Example"
    val news1 = News(symbol, DateExtended("18/06/2013").dateExtended, 2013,
      "Agilent Technologies Inc Announces Offering of Senior Notes",
      "Agilent Technologies Inc Announces Offering of Senior Notes Reuters Key Development - Jun 18, 2013"
    )
    val news2 = News(symbol, DateExtended("18/06/2013").dateExtended, 2013,
      "Agilent Technologies Inc Prices $600 Million of Senior Notes",
      "Agilent Technologies Inc Prices $600 Million of Senior Notes Reuters Key Development - Jun 18, 2013"
    )
    val news3 = News(symbol, DateExtended("02/03/2014").dateExtended, 2014,
      "Update: Agilent Technologies, Inc. Short Interest Grows by 5.8%",
      "Update: Agilent Technologies, Inc. Short Interest Grows by 5.8% Wall Street Pulse - Mar 2, 2015 Agilent Technologies, Inc. (NYSE:A) reported a rise of 132,877 shares or 5.8% in the short interest. The remaining shorts are 0.7% of the total floated shares.Share Price of Agilent Technologies, Inc. Rally 0.62% - Ashburn DailyAgilent Technologies Receives &quot;A-&quot; Credit Rating from Morningstar (A) - sleekmoney"
    )
    val companyAllNews = CompanyAllNews(
      symbol,
      Stream(news1, news2, news3)
    )

    val sentimentInOneGo: CompanyNewsSentiment = SentimentAnalyzer.evaluateSentiOfAllCompanyNews(companyAllNews)

    val sentimentAvgTitle1 = SentimentAnalyzer.findAvgSenti(Stream(
      SentimentAnalyzer.evaluateSentiOfText(news1.title),
      SentimentAnalyzer.evaluateSentiOfText(news2.title)
    ))
    val sentimentAvgTitle2 = SentimentAnalyzer.findAvgSenti(Stream(
      SentimentAnalyzer.evaluateSentiOfText(news3.title)
    ))
    val sentimentAvgDescript1 = SentimentAnalyzer.findAvgSenti(Stream(
      SentimentAnalyzer.evaluateSentiOfText(news1.description),
      SentimentAnalyzer.evaluateSentiOfText(news2.description)
    ))
    val sentimentAvgDescript2 = SentimentAnalyzer.findAvgSenti(Stream(
      SentimentAnalyzer.evaluateSentiOfText(news3.description)
    ))

    CompanyNewsSentiment(
      symbol,
      Map(DateExtended.fromString("18/06/2013") -> sentimentAvgTitle1, DateExtended.fromString("02/03/2014") -> sentimentAvgTitle2),
      Map(DateExtended.fromString("18/06/2013") -> sentimentAvgDescript1, DateExtended.fromString("02/03/2014") -> sentimentAvgDescript2)
    ) should be(sentimentInOneGo)
  }
}
