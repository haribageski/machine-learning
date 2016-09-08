package utils.cats_Show

import cats.Show
import model.{DateExtended, SymDate}
import model.dailyFinancialParameters.{CompanyDailyFinData, CompanyDailyFinParameter}
import model.SymYear
import model.yearlyFinancialParameters.{CompanyYearlyFinData, CompanyYearlyFinParameter}

object ShowDefaults {
  implicit def symYearShow: Show[SymYear] = Show.show[SymYear] { symYear =>
    s"${symYear.sym} at date:${symYear.year}"
  }
  implicit def symDateShow: Show[SymDate] = Show.show[SymDate] { symDate =>
    s"${symDate.sym} at date:${symDate.dateE}"
  }
  implicit def dateExtendedShow: Show[DateExtended] = Show.show[DateExtended] { dateExtended =>
    s"${dateExtended.dateExtended.toString}"
  }
  implicit def companyYearlyFinDataShow: Show[CompanyYearlyFinData] = Show.show[CompanyYearlyFinData] { yearlyData =>
    s"symbol: ${yearlyData.symbol};\n bookValue: ${yearlyData.bookValue};\n shares: ${yearlyData.shares};\n " +
      s"ROE: ${yearlyData.rOE};\n accrual: ${yearlyData.accrual}"
  }
  implicit def companyDailyFinDataShow: Show[CompanyDailyFinData] = Show.show[CompanyDailyFinData] { dailyData =>
//    s"dividends: ${dailyData.parameterDividends};\n" +
      s" quotes: ${dailyData.parameterQuotes};" +
      s"\n SUE: ${dailyData.parameterSUEs}"
  }
  implicit def companyYearlyFinParameterShow: Show[CompanyYearlyFinParameter] = Show.show[CompanyYearlyFinParameter] {
    yearlyParam =>
      s"symbol: ${yearlyParam.symbol}, oldest: ${yearlyParam.oldestEntryOpt}, earliest: ${yearlyParam.earliestEntryOpt}," +
        s" all entries: ${yearlyParam.allCompanyEntriesOfOneYearlyParam}"
  }
  implicit def companyDailyFinParameterShow: Show[CompanyDailyFinParameter] = Show.show[CompanyDailyFinParameter] {
    dailyParam =>
      s"symbol: ${dailyParam.symbol}, oldest: ${dailyParam.oldestEntryO.getOrElse("No oldest entry set")}," +
        s" earliest: ${dailyParam.latestEntryO.getOrElse("No earliest entry set")}," +
        s" all entries: ${dailyParam.allCompanyEntriesOfOneDailyParam}"
  }
}

