package utils

import org.joda.time.IllegalFieldValueException
import org.joda.time.format.DateTimeFormat
import org.scalatest._
import scala.collection.SortedSet
import utils.ordered.OrderedSyntax._


class DateExtendedTest extends FlatSpec with Matchers {
  val dateExtended1 =  DateExtended("01/01/2016")
  val dateExtended2 =  DateExtended("01/02/2016")
  val dateExtended3 =  DateExtended("02/02/2016")
  val dateExtended4 =  DateExtended("02/04/2016")
  val dateExtended5 =  DateExtended("02/05/2016")


  "Instance of DateExtended " should "have a correct format and DateTime" in {
    DateExtended.formatter should be(DateTimeFormat.forPattern("dd/MM/yyyy"))
    DateExtended("01/01/2016").dateExtended should be(DateTimeFormat.forPattern("dd/MM/yyyy").parseDateTime("01/01/2016"))
  }

  "DateExtended.canEqual(other)" should "return true for other of type DateExtended" in  {

    dateExtended1.canEqual(dateExtended1) should be(true)
    dateExtended1.canEqual(dateExtended2) should be(true)
    dateExtended1.canEqual(2) should be(false)
    dateExtended1.canEqual("01/01/2016") should be(false)
  }

  "DateExtended.compare(other)" should "compare this with, other used for sorting" in  {
    dateExtended1.compareTo(dateExtended1) == 0 should be(true)
    dateExtended1.compareTo(dateExtended2) < 0 should be(true)

    val tree1 = SortedSet[DateExtended](dateExtended3 , dateExtended1, dateExtended2)
    val tree2 = SortedSet[DateExtended](dateExtended1 , dateExtended2, dateExtended3)
    tree1 == tree2 should be(true)
  }

  "DateExtended.equals" should "return true for two DateExtended with same date" in {
    dateExtended1 == DateExtended("01/01/2016") should be(true)
    dateExtended1 == dateExtended2 should be(false)
  }

  "DateExtended(val: String)" should "throw IllegalArgumentException in" in {
    a [IllegalArgumentException] should be thrownBy {
      DateExtended("01-01-2016")
    }
  }

  "DateExtended(val: String)" should "throw IllegalFieldValueException in" in {
    a [IllegalFieldValueException] should be thrownBy {
      DateExtended("01/30/2016")
    }
  }
}
