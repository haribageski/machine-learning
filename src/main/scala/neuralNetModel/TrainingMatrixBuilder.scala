package neuralNetModel

import breeze.linalg.DenseMatrix
import model.{CombinedCompanyParameters, SymYear}
import model.dailyFinancialParameters.CompanyDailyFinDataEntry
import model.sentiment.Sentiment
import org.joda.time.DateTime

import scala.collection.immutable.HashSet
import scala.collection.parallel.ParMap
import scalaz.Scalaz._
import scalaz._

object TrainingMatrixBuilder {
  //We expect the input parameter to be filtered out from inconsistent entries
  def createMatrix(companies: List[CombinedCompanyParameters]): (Set[HashSet[Double]], Set[Double]) = {

    def iterate(companies: List[CombinedCompanyParameters], acc: Set[(HashSet[Double], Double)]):
    (Set[HashSet[Double]], Set[Double]) = companies match {

      case Nil =>
        (acc.map(_._1), acc.map(_._2))
      case h :: t =>
        val matrixForCompany: Set[(HashSet[Double], Double)] = createMatrixFromCompany(h)
        iterate(t, matrixForCompany ++ acc)
    }
    iterate(companies, Set.empty[(HashSet[Double], Double)])
  }


  //set of rows
  def createMatrixFromDailyParams(company: CombinedCompanyParameters): Set[(HashSet[Double], Double)] = {
    val symbol = company.symbol
    val newsTitles: Map[DateTime, Sentiment] = company.newsSentiment.map(_.avgSentiPerDateTitle).getOrElse(Map.empty)
    val newsDescript: Map[DateTime, Sentiment] = company.newsSentiment.map(_.avgSentiPerDateDescript).getOrElse(Map.empty)

    val sUE: List[CompanyDailyFinDataEntry] =
      company.extendedFinData.companyDailyFinData.parameterSUEs.allCompanyEntriesOfOneDailyParam
    val mapSUE: Map[DateTime, Double] =
      sUE.foldLeft(Map.empty[DateTime, Double])((acc, entry) => acc + (entry.date -> entry.value))

//    val dividends: List[CompanyDailyFinDataEntry] =
//      company.extendedFinData.companyDailyFinData.parameterDividends.allCompanyEntriesOfOneDailyParam
//    val mapDividends: Map[DateTime, Double] =
//      sUE.foldLeft(Map.empty[DateTime, Double])((acc, entry) => acc + (entry.date -> entry.value))

    //We should not take all the quotes in the training matrix, some are to be putted in Y
    val quotes: List[CompanyDailyFinDataEntry] =
      company.extendedFinData.companyDailyFinData.parameterQuotes.allCompanyEntriesOfOneDailyParam
    val mapQuotes: Map[DateTime, Double] =
      quotes.filter(dateWithVal => mapSUE.keySet.contains(dateWithVal.date))
      .foldLeft(Map.empty[DateTime, Double])((acc, entry) => acc + (entry.date -> entry.value))
    val resultQuotesMap: Map[DateTime, Double] =
      quotes.filter(dateWithVal => mapSUE.keySet.foldMap(date => Set(date.plusDays(1), date.plusDays(2), date.plusDays(3),
        date.plusDays(4), date.plusDays(5), date.plusDays(6))).contains(dateWithVal.date))
        .foldLeft(Map.empty[DateTime, Double])((acc, entry) => acc + (entry.date -> entry.value))


    mapSUE.keySet.zip(resultQuotesMap.keySet).map((dateTimeX_Y: (DateTime, DateTime)) => {
      val dateTimeX = dateTimeX_Y._1
      val dateTimeY = dateTimeX_Y._2
      (HashSet(
//          mapDividends(dateTimeX),
          mapSUE(dateTimeX),
          mapQuotes(dateTimeX),
          newsTitles(dateTimeX).veryPos,
          newsTitles(dateTimeX).pos,
          newsTitles(dateTimeX).neut,
          newsTitles(dateTimeX).neg,
          newsTitles(dateTimeX).veryNeg,
          newsDescript(dateTimeX).pos, newsDescript(dateTimeX).veryPos, newsDescript(dateTimeX).neut, newsDescript(dateTimeX).neg, newsDescript(dateTimeX).veryNeg
        ), resultQuotesMap(dateTimeY))
    })
  }


  def createMatrixFromYearlyParams(company: CombinedCompanyParameters, datesInDailyParams: Set[DateTime]): Set[HashSet[Double]] = {
    val symbol = company.symbol

    datesInDailyParams
      .map(date => HashSet(
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

  def createMatrixFromCompany(company: CombinedCompanyParameters): Set[(HashSet[Double], Double)] = {
    val allDates = company.extendedFinData.companyDailyFinData.parameterQuotes.allCompanyEntriesOfOneDailyParam.map(_.date).toSet
    lazy val fromDailyAndResult: Set[(HashSet[Double], Double)] = createMatrixFromDailyParams(company)
    lazy val fromYearly: Set[HashSet[Double]] = createMatrixFromYearlyParams(company, allDates)

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
