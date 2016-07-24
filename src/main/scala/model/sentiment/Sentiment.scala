package model.sentiment

import cats.Monoid

case class Sentiment(pos: Double, neg: Double, neut: Double)
