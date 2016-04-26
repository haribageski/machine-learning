package utils

/**
  * Created by hari on 13/03/16.
  */
case class SymYear(sym: String , year: Int) {
  override def hashCode: Int =
    year.hashCode()     //may not be always unique, up to milliseconds

//  override def equals(other: Any): Boolean =  other match{
//    case that: SymYear =>
//      that.canEqual(this) && sym.equals(that.sym) && year.equals(that.year)
//    case _  => false
//  }
}
