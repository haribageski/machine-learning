package neuralNetModel

import breeze.linalg.DenseMatrix
import model.{CombinedCompanyParameters, SymYear}
import model.dailyFinancialParameters.{CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import model.sentiment.Sentiment
import org.joda.time.DateTime

import scala.collection.immutable.HashSet
import scala.collection.parallel.ParMap
import scalaz.Scalaz._
import scalaz._

object TrainingMatrixBuilder {
  //We expect the input parameter to be filtered out from inconsistent entries
  def createMatrix(companies: List[CombinedCompanyParameters]): (Set[List[Double]], Set[Double]) = {

    def iterate(companies: List[CombinedCompanyParameters], acc: Set[(List[Double], Double)]):
    (Set[List[Double]], Set[Double]) = companies match {

      case Nil =>
        (acc.map(_._1), acc.map(_._2))
      case h :: t =>
        val matrixForCompany: Set[(List[Double], Double)] = createMatrixFromCompany(h)
        iterate(t, matrixForCompany ++ acc)
    }
    iterate(companies, Set.empty[(List[Double], Double)])
  }


  //set of rows
  def createMatrixFromDailyParams(company: CombinedCompanyParameters): Set[(List[Double], Double)] = {
    val symbol = company.symbol
    val dates = company.newsSentiment.map(_.dates).getOrElse(Set.empty[DateTime])
    val newsTitles: Map[DateTime, Sentiment] = dates.map(date => (date, company.newsSentiment.map(_.avgSentiPerDateTitle).get(date)))
      .foldLeft(Map.empty[DateTime, Sentiment])((acc, entry) => acc + (entry._1 -> entry._2))
    val newsDescript: Map[DateTime, Sentiment] = dates.map(date => (date, company.newsSentiment.map(_.avgSentiPerDateDescript).get(date)))
      .foldLeft(Map.empty[DateTime, Sentiment])((acc, entry) => acc + (entry._1 -> entry._2))

    val mapDividends: Map[DateTime, Double] = dates.map(date => (date, company.extendedFinData.companyDailyFinData.parameterDividends.groupedByYearM(date.getYear)))
      .map(dateList => (dateList._1, CompanyDailyFinParameter.getAvgPerYear(dateList._2)))
      .foldLeft(Map.empty[DateTime, Double])((acc, pair) => acc + (pair._1 -> pair._2))

    val mapSUE: Map[DateTime, Double] = dates.map(date => (date, company.extendedFinData.companyDailyFinData.parameterSUEs.groupedByYearM(date.getYear)))
      .map(dateList => (dateList._1, CompanyDailyFinParameter.getAvgPerYear(dateList._2)))
      .foldLeft(Map.empty[DateTime, Double])((acc, pair) => acc + (pair._1 -> pair._2))


//    val mapSUE: Map[DateTime, Double] = dates.map(date => company.extendedFinData.companyDailyFinData.parameterSUEs.groupedByYearM(date.getYear))
//      .map(list => CompanyDailyFinParameter.getAvgPerYear(list))

//    val dividends: List[CompanyDailyFinDataEntry] =
//      company.extendedFinData.companyDailyFinData.parameterDividends.allCompanyEntriesOfOneDailyParam
//    val mapDividends: Map[DateTime, Double] =
//      sUE.foldLeft(Map.empty[DateTime, Double])((acc, entry) => acc + (entry.date -> entry.value))

    //We should not take all the quotes in the training matrix, some are to be putted in Y
    val quotes: Set[CompanyDailyFinDataEntry] =
      dates.map(date => company.extendedFinData.companyDailyFinData.parameterQuotes.allCompanyEntriesOfOneDailyParam.find(_.date == date))
        .foldLeft(Set.empty[CompanyDailyFinDataEntry])((acc, x) => x match {
          case None => acc
          case Some(entry) => acc + entry
        })
    val mapQuotes: Map[DateTime, Double] =
      quotes.filter(dateWithVal => mapSUE.keySet.contains(dateWithVal.date))
      .foldLeft(Map.empty[DateTime, Double])((acc, entry) => acc + (entry.date -> entry.value))
    val resultQuotes: Set[CompanyDailyFinDataEntry] = quotes.filter(dateWithVal =>
      dates.map(date => date.plusDays(1)).contains(dateWithVal.date))

    val resultQuotesMap: Map[DateTime, Double] =
      dates.map(date => (date, resultQuotes.find(_.date == date).map(_.value)))
      .foldLeft(Map.empty[DateTime, Double])((acc, entry) =>
        entry._2 match {
          case None => acc
          case Some(value) => acc + (entry._1 -> value)
        })

    resultQuotesMap.keySet.map(date => {
      (List(
        mapDividends(date),
        mapSUE(date),
        mapQuotes(date),
        newsTitles(date).veryPos,
        newsTitles(date).pos,
        newsTitles(date).neut,
        newsTitles(date).neg,
        newsTitles(date).veryNeg,
        newsDescript(date).pos,
        newsDescript(date).veryPos,
        newsDescript(date).neut,
        newsDescript(date).neg,
        newsDescript(date).veryNeg
      ), resultQuotesMap(date))
    })
  }


  def createMatrixFromYearlyParams(company: CombinedCompanyParameters, datesInDailyParams: Set[DateTime]): Set[List[Double]] = {
    val symbol = company.symbol

    datesInDailyParams
      .map(date =>
        (
          company.extendedFinData.companyYearlyFinData.accrual.allCompanyEntriesOfOneYearlyParam.find(p => p.year == date.getYear).map(_.value),
          company.extendedFinData.companyYearlyFinData.bookValue.allCompanyEntriesOfOneYearlyParam.find(p => p.year == date.getYear).map(_.value),
          company.extendedFinData.companyYearlyFinData.rOE.allCompanyEntriesOfOneYearlyParam.find(p => p.year == date.getYear).map(_.value),
          company.extendedFinData.companyYearlyFinData.shares.allCompanyEntriesOfOneYearlyParam.find(p => p.year == date.getYear).map(_.value)
          )
      ).foldLeft(Set.empty[List[Double]])((acc, values) =>
      (values._1 |@| values._2 |@| values._3 |@| values._4) {
        (val1, val2, val3, val4) => List(val1, val2, val3, val4)
      } match {
        case None => acc
        case Some(set) => acc + set
      }
    )
//        ++
//          company.extendedFinData.companyBMratio.map(_.perYearM(SymYear(symbol, date.getYear)).value).toList
//        ++
//        company.extendedFinData.companyMarketValues.map(_.perYearM(SymYear(symbol, date.getYear)).value).toList
//        ++
//        company.extendedFinData.companySize.map(_.perYearM(SymYear(symbol, date.getYear)).value).toList
//      )
  }

  def createMatrixFromCompany(company: CombinedCompanyParameters): Set[(List[Double], Double)] = {
    val allDates = company.newsSentiment.map(_.dates).getOrElse(Set.empty[DateTime])
    lazy val fromDailyAndResult: Set[(List[Double], Double)] = createMatrixFromDailyParams(company)

    lazy val fromYearly: Set[List[Double]] = createMatrixFromYearlyParams(company, allDates)

    (fromDailyAndResult zip fromYearly).map((dailyAndYearlyList: ((List[Double], Double), List[Double])) => {
      (dailyAndYearlyList._1._1 ++ dailyAndYearlyList._2, dailyAndYearlyList._1._2)
    })
  }


  //TODO: Fix it.
  def divideToTrainingCrossValidationTest(XandY: (Set[List[Double]], Set[Double])): (Set[List[Double]], Set[List[Double]], Set[List[Double]], Set[Double], Set[Double], Set[Double]) = {
    val X = XandY._1
    val Y = XandY._2
    val total = Y.size
    val trainingSetSize = (total * 0.6).floor.toInt
    val crossValidationSetSize = ((total - trainingSetSize) * 0.2).floor.toInt
    val testSetSize = total - trainingSetSize - crossValidationSetSize
    (X.take(trainingSetSize), (X.drop(trainingSetSize)).take(crossValidationSetSize), X.drop(total - testSetSize),
      Y.take(trainingSetSize), (Y.drop(trainingSetSize)).take(crossValidationSetSize), Y drop(total - testSetSize))
  }
//  {
//    Set<String> keys = companiesMap.keySet();
//    int i = 0;
//    for(String key : keys)
//    {
//      if(i < companiesMap.size()*0.6)
//      {
//        Company tempCompany = companiesMap.get(key);
//        trainingCompaniesMap.put(key, tempCompany);
//        i++;
//      }
//      else
//      if(i >= companiesMap.size()*0.6 && i < companiesMap.size()*0.8)
//      {
//        Company tempCompany = companiesMap.get(key);
//        crossValidationCompaniesMap.put(key, tempCompany);
//        i++;
//      }
//      else
//      {
//        Company tempCompany = companiesMap.get(key);
//        testingCompaniesMap.put(key, tempCompany);
//        i++;
//      }
//    }
//  }
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
