package yearlyFinancialParametersTest

import org.scalatest._
import yearlyFinancialParameters.YearlyFinDataEntry
import scala.collection.SortedSet

class YearlyFinDataEntryTest extends FlatSpec with Matchers {
  "Instance of YearlyFinData " should "be correctly constructed" in {
    val yearlyFinData = YearlyFinDataEntry("A" , 1000, 2015)
    yearlyFinData.value should be(1000)
    yearlyFinData.symbol should be("A")
    yearlyFinData.year should be(2015)
  }

  "YearlyFinData.setVal" should "return new instance of YearlyFinData with newly set value" in {
    val yearlyFinData1 = YearlyFinDataEntry("A" , 1000, 2015)
    yearlyFinData1.setVal(1001).value should be(1001)
  }

  "Instances of YearlyFinData " should "be comparable" in {
    val yearlyFinData1 = YearlyFinDataEntry("A" , 1000, 2015)
    val yearlyFinData2 = YearlyFinDataEntry("A" , 1000, 2015)
    val yearlyFinData3 = YearlyFinDataEntry("B" , 1000, 2015)
    val yearlyFinData4 = YearlyFinDataEntry("A" , 1001, 2015)
    val yearlyFinData5 = YearlyFinDataEntry("A" , 1000, 2016)

    yearlyFinData1 == yearlyFinData2 should be(true)
    yearlyFinData1 == yearlyFinData3  should be(false)
    yearlyFinData1 == yearlyFinData4  should be(false)
    yearlyFinData1 == yearlyFinData5  should be(false)
    yearlyFinData1 < yearlyFinData3  should be(false)
    yearlyFinData1 < yearlyFinData4  should be(true)
  }

}
