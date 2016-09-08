package model.sentiment

case class Sentiment(veryPos: Double, pos: Double, neut: Double, neg: Double, veryNeg: Double)

object Sentiment{
  def apply(): Sentiment = apply(0d, 0d, 0d, 0d, 0d)
}
