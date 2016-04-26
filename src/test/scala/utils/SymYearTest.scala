package utils

import org.scalatest._
import utils.{SymYear, DateExtended}
import scala.collection.SortedSet
import utils.ordered.DefaultOrdered._

class SymYearTest extends FlatSpec with Matchers {
  val dateExtended1 =  DateExtended("01/01/2016")
  val dateExtended2 =  DateExtended("01/02/2013")
  val dateExtended3 =  DateExtended("02/02/2015")
  val dateExtended4 =  DateExtended("02/04/2014")
  val dateExtended5 =  DateExtended("02/05/2016")

  val symYear1 =  SymYear("A", dateExtended1.dateExtended.getYear)
  val symYear2 =  SymYear("B", dateExtended2.dateExtended.getYear)
  val symYear3 =  SymYear("A", dateExtended3.dateExtended.getYear)
  val symYear4 =  SymYear("D", dateExtended4.dateExtended.getYear)
  val symYear5 =  SymYear("A", dateExtended5.dateExtended.getYear)


  "Instance of SymYear " should "have a correct symbol and year" in {
    symYear1.sym should be("A")
    symYear1.year should be(2016)
  }

  "SymYear.canEqual(other)" should "return true for other of type SymYear" in  {

    symYear1.canEqual(symYear1) should be(true)
    symYear1.canEqual(symYear2) should be(true)
    symYear1.canEqual(2) should be(false)
    symYear1.canEqual("A 01/01/2016") should be(false)
  }

  "SymYear.compare(other)" should "compare this with other SymYear used for sorting" in  {
    symYear1.compareTo(symYear1) == 0 should be(true)
    symYear1.compareTo(symYear2) < 0 should be(true)
    symYear1.compareTo(symYear3) > 0 should be(true)
    symYear1.compareTo(symYear5) == 0 should be(true)

    val tree1 = SortedSet[SymYear](symYear3 , symYear1, symYear5, symYear2)
    val tree2 = SortedSet[SymYear](symYear1 , symYear2, symYear3, symYear5)
    tree1 == tree2 should be(true)
  }

  "SymYear.equals" should "return true for two symYear with same symbol name and year" in {
    symYear1 == symYear5 should be(true)
    symYear1 == symYear2 should be(false)
    symYear1 == "01/01/2016" should be(false)
    symYear1 == 1 should be(false)
  }
}
