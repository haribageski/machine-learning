package utils.readers

import scalaz.Validation

trait ReadableParameter[A] {
  def readParameterFromFile(filePath: String, symbol: String, indexOfValue: Int): Validation[String, A]
}

trait Readable[A] {
  def readDataFromFile(symbol: String): Validation[String, A]
  def getNamesOfFiles(): Set[String]
}

trait ReadableColumns {
  def readColumnsFromFile(filePath: String): Validation[String, List[List[String]]]
}


