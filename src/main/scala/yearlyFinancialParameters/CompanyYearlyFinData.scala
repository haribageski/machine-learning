package yearlyFinancialParameters

import utils.DateExtended

case class CompanyYearlyFinData (
                             symbol: String,
                             bookValue: CompanyYearlyFinParameter,
                             shares: CompanyYearlyFinParameter,
                             rOE: CompanyYearlyFinParameter,
                             accrual: CompanyYearlyFinParameter
                           ) {

  //TODO: Get filePath in other way (from config)
  private val filePath = "resources/yearly-fin-parameters/" + symbol + ".txt"

  protected def validateValueInLines(indexes: Seq[Int])(line: List[String]): Boolean = {
    indexes.forall(index => line(index).toDouble != Double.NaN && line(index) != null)
  }

  /**
    * The files to read the yearly parameters from are with lines of the following structure:
    * symbol /t date /t parameter1 /t parameter2 /t parameter3 /t parameter4
    */
  def readFromFiles(): CompanyYearlyFinData = {
    import reading_data_from_file.ReadColumns

    val indexesOfValues = Seq(2,3,4,5)
    val inputDataFromFile: List[List[String]] = ReadColumns.readColumns(filePath)

    val filteredInput: List[List[String]] = inputDataFromFile.filter(validateValueInLines(indexesOfValues))
    val filteredZeroShares = filteredInput.filter(line => line(3).toInt != 0)

    val entriesForParameterBookVal: List[YearlyFinDataEntry] =
      filteredZeroShares.map(
        line => YearlyFinDataEntry(
          symbol, line(2).toDouble, DateExtended(line(1)).dateExtended.getYear
        )
      )
    val entriesForParameterShares: List[YearlyFinDataEntry] =
      filteredZeroShares.map(
        line => YearlyFinDataEntry(
          symbol, line(3).toDouble, DateExtended(line(1)).dateExtended.getYear
        )
      )
    val entriesForParameterROEs: List[YearlyFinDataEntry] =
      filteredZeroShares.map(
        line => YearlyFinDataEntry(
          symbol, line(4).toDouble, DateExtended(line(1)).dateExtended.getYear
        )
      )
    val entriesForParameterAccruals: List[YearlyFinDataEntry] =
      filteredZeroShares.map(
        line => YearlyFinDataEntry(
          symbol, line(5).toDouble, DateExtended(line(1)).dateExtended.getYear
        )
      )


    val emptyBookValue = CompanyYearlyFinParameter(symbol)
    val emptyShares = CompanyYearlyFinParameter(symbol)
    val emptyROE = CompanyYearlyFinParameter(symbol)
    val emptyAccrual = CompanyYearlyFinParameter(symbol)

    val allBookValues: CompanyYearlyFinParameter =
      entriesForParameterBookVal.foldLeft(emptyBookValue)((acc, entryYoAdd) => acc.addEntry(entryYoAdd))
    val allShares: CompanyYearlyFinParameter =
      entriesForParameterShares.foldLeft(emptyShares)((acc, entryToAdd) => acc.addEntry(entryToAdd))
    val allROEs: CompanyYearlyFinParameter =
      entriesForParameterROEs.foldLeft(emptyROE)((acc, entryYoAdd) => acc.addEntry(entryYoAdd))
    val allAccruals: CompanyYearlyFinParameter =
      entriesForParameterAccruals.foldLeft(emptyAccrual)((acc, entryYoAdd) => acc.addEntry(entryYoAdd))

    new CompanyYearlyFinData(
      symbol,
      allBookValues,
      allShares,
      allROEs,
      allAccruals
    )
  }

  override def toString =
    s"symbol: $symbol;\n bookValue: $bookValue;\n shares: $shares;\n ROE: $rOE;\n accrual: $accrual"
}


object CompanyYearlyFinData {
  def apply(sym: String): CompanyYearlyFinData = {
    apply(
      sym, CompanyYearlyFinParameter(sym), CompanyYearlyFinParameter(sym), CompanyYearlyFinParameter(sym),
      CompanyYearlyFinParameter(sym)
    )
  }
}
