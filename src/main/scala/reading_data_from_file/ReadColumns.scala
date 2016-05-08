//package reading_data_from_file
//
//import scala.annotation.tailrec
//import scala.collection.mutable.ListBuffer
//import scala.io.Source._
//
//object ReadColumns{
//
//  def readColumns(filePath: String): List[List[String]] = {
//    val lines = readFile(filePath)
//    findColumnsFromInputLines(lines , ListBuffer.empty[List[String]])
//  }
//
//  private def readFile(filePath: String): List[String] = {
//    val source = fromFile(filePath)
//    try source.getLines.toList
//    finally source.close()
//  }
//
//  /**
//    * The lines with fields "NaN" or "" or "null" are filtered out.
//    * @param lines: List[String]
//    * @param columns: ListBuffer[List[String]
//    * @return
//    */
//  @tailrec
//  private def findColumnsFromInputLines(lines: List[String], columns: ListBuffer[List[String]]): List[List[String]] =
//    lines match {
//      case Nil => columns.reverse.toList
//      case currentLine :: tailLines =>
//        val columnsInCurrentLine: Array[String] = currentLine.split("\\t")
//        val validLine: Boolean = columnsInCurrentLine.forall {
//          case "NaN" | "" | "null" => false
//          case _ => true
//        }
//        validLine match {
//          case false => findColumnsFromInputLines(tailLines, columns)
//          case _ => findColumnsFromInputLines(tailLines, columns += columnsInCurrentLine.toList)
//        }
//    }
//}
