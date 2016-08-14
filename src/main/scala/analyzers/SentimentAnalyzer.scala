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
    val sentences: List[CoreMap] = annotation.get(classOf[CoreAnnotations.SentencesAnnotation]).asScala.toList

    if (sentences != null && sentences.nonEmpty) {
      sentences.foldLeft(SentimentMonoid.empty)((acc, sentence) => {
        val sentiment: String = sentence.get(classOf[SentimentCoreAnnotations.SentimentClass])
        println("Sentiment for :" + sentence.toString() + "is: " + sentiment)
        sentiment match {
          case "Very positive" => acc.copy(pos = acc.pos + 3)
          case "Positive" => acc.copy(pos = acc.pos + 1)
          case "Negative" => acc.copy(neg = acc.neg + 1)
          case "Neutral" => acc.copy(neut = acc.neut + 1)
          case "Very negative" => acc.copy(neg = acc.neg + 3)
        }
      })
    }
    else
      SentimentMonoid.empty
  }

  def evaluateSentiOfAllCompanyNews(allCompanyNews: CompanyAllNews): CompanyNewsSentiment = {
    val titlesSentimentWithDate: Seq[(DateTime, Sentiment)] =
      allCompanyNews.news.map(news => (news.dateOfNews, evaluateSentiOfText(news.title)))

    val descriptionsSentimentWithDate: Seq[(DateTime, Sentiment)] =
      allCompanyNews.news.map(news => (news.dateOfNews, evaluateSentiOfText(news.description)))

    val titlesSentisPerDate: Map[DateTime, Seq[Sentiment]] = titlesSentimentWithDate.groupBy(_._1)
      .mapValues(_.map(_._2))

    val descriptionsSentisPerDate: Map[DateTime, Seq[Sentiment]] = descriptionsSentimentWithDate.groupBy(_._1)
      .mapValues(_.map(_._2))

    val avgTitlesSentisPerDate = titlesSentisPerDate.mapValues(findAvgSenti)
    val avgDescriptionsSentisPerDate = descriptionsSentisPerDate.mapValues(findAvgSenti)

    CompanyNewsSentiment(allCompanyNews.symbol, avgTitlesSentisPerDate, avgDescriptionsSentisPerDate)
  }


  def findAvgSenti(sentiments: Seq[Sentiment]): Sentiment = {
    val numOfSentiments = sentiments.length
    val totalSenti: Sentiment =
      sentiments.fold(SentimentMonoid.empty)(SentimentMonoid.combine)

    val pos = totalSenti.pos match {
      case 0 => 0
      case x => x / numOfSentiments
    }
    val neg = totalSenti.neg match {
      case 0 => 0
      case x => x / numOfSentiments
    }
    val neut = totalSenti.neut match {
      case 0 => 0
      case x => x / numOfSentiments
    }
    Sentiment(pos, neg, neut)
  }
}
