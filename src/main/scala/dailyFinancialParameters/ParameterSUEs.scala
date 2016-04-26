//package dailyFinancialParameters
//
//import reading_data_from_file.ReadColumns
//import utils.DateExtended
//
//case class ParameterSUEs(dailyParameter: CompanyDailyFinParameter){
//
//  private val filePath = "resources/earning-surprises/" + dailyParameter.symbol + ".txt"
//  private val indexOfValue = 2
//
//  def addSUE(entry: DailyFinDataEntry): ParameterSUEs = {
//    val updatedCompanyDailyFinData: CompanyDailyFinParameter = dailyParameter.addEntry(entry)
//    ParameterSUEs(updatedCompanyDailyFinData)
//  }
//
//  def addSUEs(entriesL: List[DailyFinDataEntry]): ParameterSUEs = entriesL match {
//    case Nil => this
//    case h :: t => addSUE(h).addSUEs(t)
//  }
//  /**
//    * The files to read the sue from are with lines of the following structure:
//    * symbol /t date /t value
//    */
//  def readCompanySUEs(): CompanyDailyFinParameter = {
//    dailyParameter.readCompanyEntries(filePath, indexOfValue)
//  }
//}
//
//object ParameterSUEs {
//  def apply(sym: String): ParameterSUEs = {
//    apply(CompanyDailyFinParameter(sym))
//  }
//}
