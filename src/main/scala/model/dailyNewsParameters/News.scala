package model.dailyNewsParameters

import model.DateExtended

case class News(sym: String, dateOfNews: DateExtended, yearOfNews: Int, title: String, description: String,
                isRelevant: Boolean = true, source: String = "")
