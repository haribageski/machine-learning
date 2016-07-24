package dailyNewsParameter

import utils.DateExtended

import scala.collection.immutable.HashMap

class CompanyNewsSentiment(sym: String, avgSentiPerDateTitle:  HashMap[DateExtended, Double],
                           avgSentiPerDateDescript: HashMap[DateExtended, Double], sentimentForTitles: Double,
                           sentimentForDescriptions: Double, filePathToTextNews: String,
                           filePathToSentimentOfNews: String, outputPath: String
                           //static StanfordCoreNLP _pipeline = null;
                          )
