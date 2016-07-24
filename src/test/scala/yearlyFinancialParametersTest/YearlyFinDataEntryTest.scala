package yearlyFinancialParametersTest

import org.scalatest._
import model.yearlyFinancialParameters.CompanyYearlyFinDataEntry
import scala.collection.SortedSet

class YearlyFinDataEntryTest extends FlatSpec with Matchers {
  "Instance of YearlyFinData " should "be correctly constructed" in {
    val yearlyFinData = CompanyYearlyFinDataEntry("A" , 1000, 2015)
    yearlyFinData.value should be(1000)
    yearlyFinData.symbol should be("A")
    yearlyFinData.year should be(2015)
  }

  "YearlyFinData.setVal" should "return new instance of YearlyFinData with newly set value" in {
    val yearlyFinData1 = CompanyYearlyFinDataEntry("A" , 1000, 2015)
    yearlyFinData1.setVal(1001).value should be(1001)
    yearlyFinData1.setVal(1001).year should be(2015)
    yearlyFinData1.setVal(1001).symbol should be("A")
  }

  "YearlyFinData.setYear" should "return new instance of YearlyFinData with newly set year" in {
    val yearlyFinData1 = CompanyYearlyFinDataEntry("A" , 1000, 2015)
    yearlyFinData1.setYear(2016).value should be(1000)
    yearlyFinData1.setYear(2016).year should be(2016)
    yearlyFinData1.setYear(2016).symbol should be("A")
  }

  "Instances of YearlyFinData " should "be comparable" in {
    import utils.ordered.OrderedSyntax._
    val yearlyFinData1 = CompanyYearlyFinDataEntry("A" , 1000, 2015)
    val yearlyFinData2 = CompanyYearlyFinDataEntry("A" , 1000, 2015)
    val yearlyFinData3 = CompanyYearlyFinDataEntry("B" , 1000, 2015)
    val yearlyFinData4 = CompanyYearlyFinDataEntry("A" , 1001, 2015)
    val yearlyFinData5 = CompanyYearlyFinDataEntry("A" , 1000, 2016)

    yearlyFinData1 == yearlyFinData2 should be(true)
    yearlyFinData1 == yearlyFinData3  should be(false)
    yearlyFinData1 == yearlyFinData4  should be(false)
    yearlyFinData1 == yearlyFinData5  should be(false)
    yearlyFinData1 < yearlyFinData3  should be(false)
    yearlyFinData1 < yearlyFinData4  should be(true)
  }

}
