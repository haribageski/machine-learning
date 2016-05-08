//package reading_data_from_file
//
//import scala.annotation.tailrec
//import scala.io.Source._
//
//object ReadColumnWithIndexFromFile{
//
//
//  def readOneColumn(filePath: String, indexOfColumnToBeRead: Int): List[String] = {
//    val lines = readFile(filePath)
//    val emptyColumn: List[String] = Nil
//    findColumnFromInputLines(lines, indexOfColumnToBeRead, emptyColumn)
//  }
//
//  private def readFile(fileName: String): List[String] = {
//    val source = fromFile(fileName)
//    try source.getLines.toList
//    finally source.close()
//  }
//
//  @tailrec
//  private def findColumnFromInputLines(lines: List[String], indexOfColumn: Int, column: List[String]): List[String] =
//    lines match {
//      case Nil => column.reverse
//      case currentLine :: tailLines =>
//        val columnsInCurrentLine: Array[String] = currentLine.split("\\t")
//        val field: String = columnsInCurrentLine(indexOfColumn)
//        field match {
//          case "NaN" | "" | "null" => findColumnFromInputLines(tailLines, indexOfColumn, column)
//          case _ => findColumnFromInputLines(tailLines, indexOfColumn, field :: column)
//        }
//    }
//}
