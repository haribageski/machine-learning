package utils.cats_Show

import cats.Show
import utils.{DateExtended, SymDate, SymYear}

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
}

