package neuralNetModel

import model.{CombinedCompanyParameters, SymYear}
import model.dailyFinancialParameters.CompanyDailyFinDataEntry
import model.sentiment.Sentiment
import model.yearlyFinancialParameters.CompanyYearlyFinDataEntry
import org.joda.time.DateTime

object TrainingMatrixBuilder {
  //We expect the input parameter to be filtered out from inconsistent entries
  def createMatrix(companies: List[CombinedCompanyParameters]): (Set[List[Double]], Set[Double]) = {

    def iterate(companies: List[CombinedCompanyParameters], acc: Set[(List[Double], Double)]):
    (Set[List[Double]], Set[Double]) = companies match {

      case Nil => (acc.map(_._1), acc.map(_._2))

      case h :: t =>
        val matrixForCompabt: Set[(List[Double], Double)] = createMatrixFromCompany(h)
        iterate(t, matrixForCompabt ++ acc)
    }
    iterate(companies, Set.empty)
  }


  //set of rows
  def createMatrixFromDailyParams(company: CombinedCompanyParameters): Set[(List[Double], Double)] = {
    val symbol = company.symbol
    val newsTitles: Map[DateTime, Sentiment] = company.newsSentiment.avgSentiPerDateTitle
    val newsDescript: Map[DateTime, Sentiment] = company.newsSentiment.avgSentiPerDateDescript

    val sUE: List[CompanyDailyFinDataEntry] =
      company.extendedFinData.companyDailyFinData.parameterSUEs.allCompanyEntriesOfOneDailyParam
    val mapSUE: Map[DateTime, Double] =
      sUE.foldLeft(Map.empty[DateTime, Double])((acc, entry) => acc + (entry.date -> entry.value))

    val dividends: List[CompanyDailyFinDataEntry] =
      company.extendedFinData.companyDailyFinData.parameterDividends.allCompanyEntriesOfOneDailyParam
    val mapDividends: Map[DateTime, Double] =
      dividends.foldLeft(Map.empty[DateTime, Double])((acc, entry) => acc + (entry.date -> entry.value))

    //We should not take all the quotes in the training matrix, some are to be putted in Y
    val quotes: List[CompanyDailyFinDataEntry] =
      company.extendedFinData.companyDailyFinData.parameterQuotes.allCompanyEntriesOfOneDailyParam
    val mapQuotes: Map[DateTime, Double] =
      quotes.foldLeft(Map.empty[DateTime, Double])((acc, entry) => acc + (entry.date -> entry.value))
        .filter(dateWithVal => mapDividends.keySet.contains(dateWithVal._1))

    val resultQuotesMap: Map[DateTime, Double] =
      quotes.foldLeft(Map.empty[DateTime, Double])((acc, entry) => acc + (entry.date -> entry.value))
        .filter(dateWithVal => mapQuotes.contains(dateWithVal._1.minusDays(1)))

    mapSUE.keySet.map(dateTime =>
      (List(mapDividends(dateTime), mapSUE(dateTime), mapQuotes(dateTime),
        newsTitles(dateTime).neut, newsTitles(dateTime).neg, newsTitles(dateTime).pos,
        newsDescript(dateTime).neut, newsDescript(dateTime).neg, newsDescript(dateTime).pos),
        resultQuotesMap(dateTime.plusDays(1)))
    )
  }


  def createMatrixFromYearlyParams(company: CombinedCompanyParameters, datesInDailyParams: Set[DateTime]): Set[List[Double]] = {
    val symbol = company.symbol

    datesInDailyParams
      .map(date => List(
          company.extendedFinData.companyYearlyFinData.accrual.perYearM(SymYear(symbol, date.getYear)).value,
          company.extendedFinData.companyYearlyFinData.bookValue.perYearM(SymYear(symbol, date.getYear)).value,
          company.extendedFinData.companyYearlyFinData.rOE.perYearM(SymYear(symbol, date.getYear)).value,
          company.extendedFinData.companyYearlyFinData.shares.perYearM(SymYear(symbol, date.getYear)).value
      ))
//        ++
//          company.extendedFinData.companyBMratio.map(_.perYearM(SymYear(symbol, date.getYear)).value).toList
//        ++
//        company.extendedFinData.companyMarketValues.map(_.perYearM(SymYear(symbol, date.getYear)).value).toList
//        ++
//        company.extendedFinData.companySize.map(_.perYearM(SymYear(symbol, date.getYear)).value).toList
//      )
  }

  def createMatrixFromCompany(company: CombinedCompanyParameters): Set[(List[Double], Double)] = {
    val allDates = company.extendedFinData.companyDailyFinData.parameterQuotes.allCompanyEntriesOfOneDailyParam.map(_.date).toSet
    val fromDailyAndResult: Set[(List[Double], Double)] = createMatrixFromDailyParams(company)
    val fromYearly: Set[List[Double]] = createMatrixFromYearlyParams(company, allDates)

    (fromDailyAndResult zip fromYearly).map(dailyAndYearlyList =>
      (dailyAndYearlyList._1._1 ++ dailyAndYearlyList._2, dailyAndYearlyList._1._2)
    )
  }
}


/*
	//System.out.println("containsKey:" + Companies_match.get(Sym).get_Fin_fundamentals().get_all_company_book_val().containsKey(S_Y_prev));
					X[0][i-skipped] = 1.0;// Companies_match.get(Sym).get_Company_SUE().Get_avg_per_y_SUE().get(year-1);
					X[1][i-skipped] = mapOfCompanies.get(Sym).get_Company_Dividend().get_avg_dividends().get(year-1);
					X[2][i-skipped] = mapOfCompanies.get(Sym).get_Fin_fundamentals().getAllCompanyBookVal().get(S_Y_prev).getVal();
					X[3][i-skipped] = mapOfCompanies.get(Sym).get_Fin_fundamentals().getAllCompanyShares().get(S_Y_prev).getVal();
					X[4][i-skipped] = mapOfCompanies.get(Sym).get_Fin_fundamentals().getAllCompanyROE().get(S_Y_prev).getVal();
					X[5][i-skipped] = mapOfCompanies.get(Sym).get_Fin_fundamentals().getAllCompanyAccrual().get(S_Y_prev).getVal();
					X[6][i-skipped] = mapOfCompanies.get(Sym).get_Fin_fundamentals().getAllCompanyMarketVals().get(S_Y_prev);
					X[7][i-skipped] = mapOfCompanies.get(Sym).get_Fin_fundamentals().getAllBM_Ratios().get(S_Y_prev);
					X[8][i-skipped] = mapOfCompanies.get(Sym).get_Company_Qoutes().get_quotes_map().get(S_D.get_Date_modif().get_prev_day_as_datemodif());
					X[9][i-skipped] = mapOfCompanies.get(Sym).get_Fin_fundamentals().getAllCompanyYearSizes().get(S_Y_prev);

					if(Y!=null)
						Y[i-skipped] = mapOfCompanies.get(Sym).get_Company_Qoutes().get_quotes_map().get(S_D.get_Date_modif());
					//System.out.println(Companies_match.get(Sym) + " added for training");
 */
