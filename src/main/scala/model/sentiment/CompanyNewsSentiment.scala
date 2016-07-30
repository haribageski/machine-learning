package model.sentiment

import model.DateExtended
import org.joda.time.DateTime

import scala.collection.immutable.HashMap

case class CompanyNewsSentiment(
                            sym: String,
                            avgSentiPerDateTitle:  Map[DateTime, Sentiment],
                            avgSentiPerDateDescript: Map[DateTime, Sentiment]
                           //static StanfordCoreNLP _pipeline = null;
                          )
