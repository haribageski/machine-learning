package utils

import org.joda.time.DateTime
import org.joda.time.format._
import scala.language.implicitConversions

/**
  * Ordered Joda DateTime in format dd/MM/yyyy.
  */
case class DateExtended (date: String) {
  val dateExtended: DateTime = DateExtended.fromString(date)

  /**
    * @return negative value if this is less, 0 if equal, or positive value if greater
    * @throws NullPointerException if the object is null
    * @throws ClassCastException if the object type is not supported
    */
  override def equals(that: Any) = that match {
    case that: DateExtended => dateExtended.equals(that.dateExtended)
    case _ => false
  }
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
