package utils.readers

import dailyFinancialParameters.{CompanyDailyFinData, CompanyDailyFinDataEntry, CompanyDailyFinParameter}
import utils.DateExtended
import yearlyFinancialParameters.{CompanyYearlyFinData, CompanyYearlyFinParameter, CompanyYearlyFinDataEntry}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.Source._
import better.files._
import java.io.{File => JFile}

trait ReadableParameter[A] {
  def readParameterFromFile(filePath: String, symbol: String, indexOfValue: Int): A
}

trait Readable[A] {
  def readDataFromFile(symbol: String): A
}

trait ReadableColumns {
  def readColumnsFromFile(filePath: String): List[List[String]]
}


