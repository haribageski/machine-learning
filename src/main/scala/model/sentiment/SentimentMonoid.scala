package model.sentiment

import cats.Monoid

object SentimentMonoid extends Monoid[Sentiment] {
  override def empty: Sentiment = Sentiment(0, 0, 0, 0, 0)
  def combine(x: Sentiment, y: Sentiment): Sentiment =
    Sentiment(x.veryPos + y.veryPos, x.pos + y.pos, x.neut + y.neut, x.neg + y.neg, x.veryNeg + y.veryNeg)
}
