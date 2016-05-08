package utils

import org.joda.time.DateTime
import org.joda.time.format._

/**
  * Ordered Joda DateTime in format dd/MM/yyyy.
  */
case class DateExtended (date: String) {
  val dateExtended: DateTime = DateExtended.fromString(date)

  override def hashCode: Int = dateExtended.hashCode
}



object DateExtended {
  val formatter = DateTimeFormat.forPattern("dd/MM/yyyy")

  /**
    * @param dateS  the date to parse, not null
    * @return the parsed date-time, never null
    * @throws UnsupportedOperationException if parsing is not supported
    * @throws IllegalArgumentException if the text to parse is invalid
    */
implicit def fromString(dateS : String): DateTime =
    formatter.parseDateTime(dateS)
}
