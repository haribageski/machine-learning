package utils.readers

import better.files.File

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object ReadableColumnsDefaults {
  implicit object ColumnsReader extends ReadableColumns {
    def readColumnsFromFile(filePath: String): List[List[String]] = {
      val lines: Traversable[String] = readFile(filePath)
      findColumnsFromInputLines(lines, ListBuffer.empty[List[String]])
    }

    private def readFile(filePath: String): Traversable[String] = {
      val file = File(filePath)
      file.lines
    }

    /**
      * The lines with fields "NaN" or "" or "null" are filtered out.
      *
      * @param lines   : List[String]
      * @param columns : ListBuffer[List[String]
      */
    private def findColumnsFromInputLines(lines: Traversable[String], columns: ListBuffer[List[String]]): List[List[String]] =
      lines.foldLeft(columns)((acc, str) => {
        val columnsInCurrentLine: Array[String] = str.split("\\t")
        val validLine: Boolean = columnsInCurrentLine.forall { p =>
          p != "NaN" && !p.isEmpty && p != "\"\"" && p != "null"
        }
        validLine match {
          case false => acc
          case true => acc += columnsInCurrentLine.toList
        }
      }).reverse.toList
  }
}
