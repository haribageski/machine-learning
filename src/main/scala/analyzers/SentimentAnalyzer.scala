package analyzers

import java.util.Properties

import scala.collection.JavaConverters._
import edu.stanford.nlp.ling.CoreAnnotations
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.sentiment.SentimentCoreAnnotations
import edu.stanford.nlp.util.CoreMap
import model.sentiment._
import model.dailyNewsParameters.CompanyAllNews
import org.joda.time.DateTime
import utils.Utils


object SentimentAnalyzer {
  lazy val pipeline = initiateStanfordNLP

  def initiateStanfordNLP(): StanfordCoreNLP = {
    val props: Properties = new Properties()
    props.put("annotators",
      "tokenize, ssplit, pos, lemma, parse, sentiment, ner")
//    props.put("dcoref.score", true)   TODO: This may be needed, check it again
    new StanfordCoreNLP(props)
  }

  /**
    * Finds number of positive, negative, and neutral sentences for the input text.
    * Sentiment represents to the number of pos,neg,neut sentences.
    */
  def evaluateSentiOfText(text: String): Sentiment = {
    val annotation: Annotation = new Annotation(text) //Annotation is a Map
    pipeline.annotate(annotation)

    // An Annotation is a Map and you can get and use the various analyses individually.
    // For instance, get the parse tree of the first sentence in the text.
    val sentences: Seq[CoreMap] = annotation.get(classOf[CoreAnnotations.SentencesAnnotation]).asScala

    if (sentences != null && sentences.nonEmpty) {
      sentences.map(coreMap => {
        val sentiment: String = coreMap.get(classOf[SentimentCoreAnnotations.SentimentClass])
//        println("Sentiment for :" + coreMap.toString() + "is: " + sentiment)
        sentiment match {
          case "Very positive" => Sentiment(1, 0, 0, 0, 0)
          case "Positive" => Sentiment(0, 1, 0, 0, 0)
          case "Neutral" => Sentiment(0, 0, 1, 0, 0)
          case "Negative" => Sentiment(0, 0, 0, 1, 0)
          case "Very negative" => Sentiment(0, 0, 0, 0, 1)
        }
      }).fold(SentimentMonoid.empty)(SentimentMonoid.combine)
    }
    else
      SentimentMonoid.empty
  }

  def evaluateSentiOfAllCompanyNews(allCompanyNews: CompanyAllNews): CompanyNewsSentiment = {
    val titlesSentimentWithDate: Stream[(DateTime, Sentiment)] =
      allCompanyNews.news.map(news => (news.dateOfNews, evaluateSentiOfText(news.title)))

    val descriptionsSentimentWithDate: Stream[(DateTime, Sentiment)] =
      allCompanyNews.news.map(news => (news.dateOfNews, evaluateSentiOfText(news.description)))

    val titlesSentisPerDate: Map[DateTime, Stream[Sentiment]] = titlesSentimentWithDate.groupBy(_._1)
      .mapValues(_.map(_._2))

    val descriptionsSentisPerDate: Map[DateTime, Stream[Sentiment]] = descriptionsSentimentWithDate.groupBy(_._1)
      .mapValues(_.map(_._2))

    val avgTitlesSentisPerDate: Map[DateTime, Sentiment] = titlesSentisPerDate.mapValues(findAvgSenti)
    val avgDescriptionsSentisPerDate: Map[DateTime, Sentiment] = descriptionsSentisPerDate.mapValues(findAvgSenti)

    CompanyNewsSentiment(allCompanyNews.symbol, avgTitlesSentisPerDate, avgDescriptionsSentisPerDate, avgDescriptionsSentisPerDate.keySet)
  }


  def findAvgSenti(sentiments: Stream[Sentiment]): Sentiment = {

    val numOfSentiments = sentiments.length.toDouble

    def findAvg: Double => Double = {
      (totalSenti: Double) => totalSenti match {
        case 0 => 0
        case x: Double => x / numOfSentiments
      }
    }

    def combine(x: Sentiment, acc: => Sentiment): Sentiment = SentimentMonoid.combine(x, acc)
    val totalSenti: Sentiment =
      Utils.foldr(combine, SentimentMonoid.empty)(sentiments)
//      sentiments.fold(SentimentMonoid.empty)(SentimentMonoid.combine)

    val veryPos = findAvg(totalSenti.veryPos)
    val pos = findAvg(totalSenti.pos)
    val neut = findAvg(totalSenti.neut)
    val neg = findAvg(totalSenti.neg)
    val veryNeg = findAvg(totalSenti.veryNeg)

    Sentiment(veryPos, pos, neut, neg, veryNeg)
  }
}
