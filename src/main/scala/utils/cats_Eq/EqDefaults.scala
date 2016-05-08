package utils.cats_Eq

import cats.Eq
import cats.syntax.eq._
import dailyFinancialParameters.CompanyDailyFinDataEntry
import utils.{DateExtended, SymDate, SymYear}
import yearlyFinancialParameters.CompanyYearlyFinDataEntry


object EqDefaults {
  implicit val eqSymYear: Eq[SymYear] = Eq.instance[SymYear] { (symYear1, symYear2) =>
    import cats.std.int._
    import cats.std.string._
    (symYear1.sym === symYear2.sym) && (symYear1.year === symYear2.year)
  }


  /**
    * @return negative value if this is less, 0 if equal, or positive value if greater
    * @throws NullPointerException if the object is null
    * @throws ClassCastException if the object type is not supported
    */
  implicit val eqDateExtended: Eq[DateExtended] = Eq.instance[DateExtended] {
    (dateExtended1: DateExtended, dateExtended2: DateExtended) =>
      dateExtended1.dateExtended.equals(dateExtended2.dateExtended)
  }

  implicit val eqSymDate: Eq[SymDate] = Eq.instance[SymDate] { (symDate1, symDate2) =>
    import cats.std.string._
    (symDate1.sym === symDate1.sym) && (symDate1.dateE  === symDate2.dateE)
  }


  implicit val eqYearlyFinDataEntry: Eq[CompanyYearlyFinDataEntry] = Eq.instance[CompanyYearlyFinDataEntry] { (entry1, entry2) =>
    import cats.std.string._
    import cats.std.int._
    import cats.std.double._
    (entry1.symbol === entry2.symbol) && (entry1.value === entry2.value) && (entry1.year === entry2.year)
  }

  implicit val eqDailyFinDataEntry: Eq[CompanyDailyFinDataEntry] =
    Eq.instance[CompanyDailyFinDataEntry] { (entry1, entry2) =>
    import cats.std.string._
    import cats.std.double._
    (entry1.symbol === entry2.symbol) && (entry1.value === entry2.value)
  }
}

