package model.dailyNewsParameters

import org.joda.time.DateTime

case class News(sym: String, dateOfNews: DateTime, yearOfNews: Int, title: String, description: String,
                isRelevant: Boolean = true, source: String = "")
