package analyzers

import java.util
import java.util.Properties

import cats.{Monad, Monoid}

import scala.collection.JavaConverters._
import edu.stanford.nlp.ling.CoreAnnotations
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.sentiment.SentimentCoreAnnotations
import edu.stanford.nlp.util.CoreMap
import model.sentiment._
import utils.readers.ReadableDefaults.CompanyNewsReader
import cats.syntax.semigroup._
import model.DateExtended
import model.dailyNewsParameters.CompanyAllNews


object SentimentAnalyzer {
  lazy val pipeline = initiateStanfordNLP

  def initiateStanfordNLP(): StanfordCoreNLP = {
    val props: Properties = new Properties()
    props.put("annotators",
      "tokenize, ssplit, pos, lemma, parse, sentiment, ner, dcoref")
    props.put("dcoref.score", "true")
    new StanfordCoreNLP(props)
  }

  /**
    * Finds number of positive, negative, and neutral sentences for the input text
    *
    * @param text
    * @return Double[3] corresponding to the number of pos,neg,neut sentences
    */
  def evaluateSentiOfText(text: String): Sentiment = {
    println("evaluateSentiOfText() - " + text)
    //    double pos_sentences = 0.0, neg_sentences = 0.0, neut_sentences =  0.0;
    //    Double[] sentiForTheThreeKinds = new Double[3];
    val annotation: Annotation = new Annotation(text) //Annotation is a Map
    pipeline.annotate(annotation)

    // An Annotation is a Map and you can get and use the various analyses individually.
    // For instance, get the parse tree of the first sentence in the text.
    val sentences: List[CoreMap] = annotation.get(classOf[CoreAnnotations.SentencesAnnotation]).asScala.toList

    if (sentences != null && !sentences.isEmpty) {
      sentences.foldLeft(monoid.empty)((acc, sentence) => {
        val sentiment = sentence.get(classOf[SentimentCoreAnnotations.SentimentClass])
        println("Sentiment for :" + sentence.toString() + "is: " + sentiment)
        sentiment match {
          case "Positive" =>
            println("Positive sentences after increment: " + acc.pos)
            acc.copy(pos = acc.pos + 1)
          case "Negative" =>
            println("Negative sentences after increment: " + acc.neg)
            acc.copy(neg = acc.neg + 1)
          case "Negative" =>
            println("Negative sentences after increment: " + acc.neut)
            acc.copy(neut = acc.neut + 1)
        }
        acc
      })
    }
    else
      Sentiment(0, 0, 0)
  }



  def evaluateSentiOfAllCompanyNews(symbol: String): CompanyNewsSentiment = {
    val allCompanyNews: CompanyAllNews = CompanyNewsReader.readDataFromFile(symbol)

    val dates: List[DateExtended] = allCompanyNews.news.map(_.dateOfNews)
    val titlesSentiment: List[(DateExtended, Sentiment)] =
      allCompanyNews.news.map(news => (news.dateOfNews, evaluateSentiOfText(news.title)))
    val descriptionsSentiment: List[(DateExtended, Sentiment)] =
      allCompanyNews.news.map(news => (news.dateOfNews, evaluateSentiOfText(news.description)))

    val titlesSentisPerDate: Map[DateExtended, List[Sentiment]] =
      titlesSentiment.groupBy(_._1).map(pair => (pair._1, pair._2.map(_._2)))

    val descriptionsSentisPerDate: Map[DateExtended, List[Sentiment]] =
      titlesSentiment.groupBy(_._1).map(pair => (pair._1, pair._2.map(_._2)))

    val avgTitlesSentisPerDate = titlesSentisPerDate.map(pair => (pair._1, findAvgSenti(pair._2)))
    val avgDescriptionsSentisPerDate = descriptionsSentisPerDate.map(pair => (pair._1, findAvgSenti(pair._2)))

    CompanyNewsSentiment(symbol, avgTitlesSentisPerDate, avgDescriptionsSentisPerDate)
  }


  def findAvgSenti(sentiments: List[Sentiment]): Sentiment = {
    val numOfSentiments = sentiments.length
    val totalSenti =
      sentiments.foldLeft(monoid.empty)(monoid.combine)

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

object monoid extends Monoid[Sentiment] {
  override def empty: Sentiment = Sentiment(0, 0, 0)
    def combine(x: Sentiment, y: Sentiment): Sentiment =
      Sentiment(x.pos + y.pos, x.neg + y.neg, x.neut + y.neut)
}
