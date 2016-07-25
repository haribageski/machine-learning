package analyzers

import cats.Monoid
import model.sentiment.Sentiment

object SentimentMonoid extends Monoid[Sentiment] {
  override def empty: Sentiment = Sentiment(0, 0, 0)
  def combine(x: Sentiment, y: Sentiment): Sentiment =
    Sentiment(x.pos + y.pos, x.neg + y.neg, x.neut + y.neut)
}
