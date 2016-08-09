package neuralNetModel

import org.scalatest.{FlatSpec, Matchers}
import utils.readers.ReadableDefaults._
import filters.DefaultFilterData._
import filters.FilterSyntax.FilterOps
import model.{CombinedCompanyParameters, DateExtended}
import neuralNetModel.TrainingMatrixBuilder._


class TrainingMatrixBuilderSpec  extends FlatSpec with Matchers {
  "createMatrix()" should "return a tuple of set of rows, each row being a list, and set of results - Quote values" in {
    val symbol = "Example"
    val combinedNonFiltered = CombinedCompanyParametersReader.readDataFromFile(symbol)
    val combinedFilteredWithDerivedParams: CombinedCompanyParameters = combinedNonFiltered.filter

    val matrix: (Set[List[Double]], Set[Double]) = createMatrix(List(combinedFilteredWithDerivedParams))
    1 should be (1)
  }
}
