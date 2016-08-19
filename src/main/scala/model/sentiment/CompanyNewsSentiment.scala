package model.sentiment

import model.DateExtended
import org.joda.time.DateTime

import scala.collection.immutable.HashMap

case class CompanyNewsSentiment(
                            sym: String,
                            avgSentiPerDateTitle:  Map[DateTime, Sentiment],
                            avgSentiPerDateDescript: Map[DateTime, Sentiment],
                            dates: Set[DateTime]
                           //static StanfordCoreNLP _pipeline = null;
                          )

object CompanyNewsSentiment {
  def apply(sym: String, dates: Set[DateTime]): CompanyNewsSentiment =
    apply(sym, Map.empty, Map.empty, dates)
}
