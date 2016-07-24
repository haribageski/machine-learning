package model.sentiment

import model.DateExtended

import scala.collection.immutable.HashMap

case class CompanyNewsSentiment(
                            sym: String,
                            avgSentiPerDateTitle:  Map[DateExtended, Sentiment],
                            avgSentiPerDateDescript: Map[DateExtended, Sentiment]
                           //static StanfordCoreNLP _pipeline = null;
                          )
