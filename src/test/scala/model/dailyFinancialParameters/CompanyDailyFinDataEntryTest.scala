package model.dailyFinancialParameters

import model.DateExtended
import org.scalatest._
import utils.ordered.OrderedSyntax._

import scala.collection.immutable.TreeSet

/**
  * Created by hari on 16/03/16.
  */
class CompanyDailyFinDataEntryTest extends FlatSpec with Matchers {
  "Instance of DailyFinData " should "be correctly constructed" in {
    val dailyFinData = CompanyDailyFinDataEntry("A", 1000, DateExtended.fromString("01/01/2015"))
    dailyFinData.value should be(1000)
    dailyFinData.symbol should be("A")
    dailyFinData.date should be(DateExtended.fromString("01/01/2015"))
  }

  "DailyFinData.setVal" should "return new instance of DailyFinData with newly set value" in {
    val dailyFinData = CompanyDailyFinDataEntry("A", 1000, DateExtended.fromString("01/01/2015"))
    dailyFinData.setVal(1001).value should be(1001)
  }

  "DailyFinData.setDate" should "return new instance of DailyFinData with newly set date" in {
    val dailyFinData = CompanyDailyFinDataEntry("A", 1000, DateExtended.fromString("01/01/2015"))
    dailyFinData.setDate(DateExtended.fromString("01/01/2014")).date should be(DateExtended.fromString("01/01/2014"))
  }

  "Instances of DailyFinData " should "be comparable" in {
    val dailyFinData1 = CompanyDailyFinDataEntry("A", 1000, DateExtended.fromString("01/01/2015"))
    val dailyFinData2 = CompanyDailyFinDataEntry("A", 1000, DateExtended.fromString("01/01/2015"))
    val dailyFinData3 = CompanyDailyFinDataEntry("B", 1000, DateExtended.fromString("01/01/2015"))
    val dailyFinData4 = CompanyDailyFinDataEntry("A", 1001, DateExtended.fromString("01/01/2015"))
    val dailyFinData5 = CompanyDailyFinDataEntry("A", 1000, DateExtended.fromString("01/01/2016"))

    dailyFinData1 == dailyFinData2 should be(true)
    dailyFinData1 == dailyFinData3 should be(false)
    dailyFinData1 == dailyFinData4 should be(true)
    dailyFinData1 == dailyFinData5 should be(false)
    dailyFinData1 == "01/01/2015" should be(false)
    TreeSet(dailyFinData1, dailyFinData3, dailyFinData5) should be (TreeSet(dailyFinData3, dailyFinData5, dailyFinData1))
  }
}
