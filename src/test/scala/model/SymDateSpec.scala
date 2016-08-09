package model

import org.joda.time.IllegalFieldValueException
import org.scalatest._
import utils.ordered.OrderedSyntax._

import scala.collection.SortedSet

class SymDateSpec extends FlatSpec with Matchers {
  val dateExtended1 =  DateExtended("01/01/2016")
  val dateExtended2 =  DateExtended("01/02/2016")
  val dateExtended3 =  DateExtended("02/02/2016")
  val dateExtended4 =  DateExtended("02/04/2016")
  val dateExtended5 =  DateExtended("02/05/2016")

  val symDate1 =  SymDate("A", dateExtended1)
  val symDate2 =  SymDate("B", dateExtended2)
  val symDate3 =  SymDate("C", dateExtended3)
  val symDate4 =  SymDate("D", dateExtended4)
  val symDate5 =  SymDate("A", dateExtended5)
  val symDate6 =  SymDate("A", dateExtended1)


  "Instance of SymDate " should "have a correct symbol and DateTime" in {
    symDate1.sym should be("A")
    symDate1.dateE should be(dateExtended1)
  }

  "SymDate.canEqual(other)" should "return true for other of type SymDate" in  {

    symDate1.canEqual(symDate1) should be(true)
    symDate1.canEqual(symDate2) should be(true)
    symDate1.canEqual(2) should be(false)
    symDate1.canEqual("A 01/01/2016") should be(false)
  }

  "SymDate.compare(other)" should "compare this with other SymDate, used for sorting" in  {
    symDate1.compareTo(symDate1) == 0 should be(true)
    symDate1.compareTo(symDate2) < 0 should be(true)
    symDate1.compareTo(symDate5) < 0 should be(true)

    val tree1 = SortedSet[SymDate](symDate3 , symDate1, symDate5, symDate2)
    val tree2 = SortedSet[SymDate](symDate1 , symDate2, symDate3, symDate5)
    tree1 == tree2 should be(true)
  }

  "SymDate.equals" should "return true for two DateExtended with same symbol name and date" in {

    symDate1 == symDate1 should be(true)
    symDate1 == symDate6 should be(true)
    symDate1 == symDate2 should be(false)
    symDate1 == "01/01/2016" should be(false)
    symDate2 == 1 should be(false)
  }

  "SymDate(val: String, date: DateExtended)" should "throw IllegalArgumentException in" in {
    a [IllegalArgumentException] should be thrownBy {
      SymDate("A" , DateExtended("01-01-2016"))
    }
  }

  "SymDate(val: String, date: DateExtended)" should "throw IllegalFieldValueException in" in {
    a [IllegalFieldValueException] should be thrownBy {
      SymDate("A" , DateExtended("01/30/2016"))
    }
  }
}
