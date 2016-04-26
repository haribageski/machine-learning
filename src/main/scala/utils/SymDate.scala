package utils

/**
  * Created by hari on 13/03/16.
  */
case class SymDate(sym: String, dateE: DateExtended)  {
  override def hashCode: Int =
    dateE.hashCode()     //may not be always unique, up to milliseconds


//  override def equals(other: Any): Boolean =  other match {
//    case that: SymDate =>
//      that.canEqual(this) && sym.equals(that.sym) && dateE.equals(that.dateE)
//    case _ => false
//  }
}


