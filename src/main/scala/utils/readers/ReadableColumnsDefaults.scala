package utils.readers

import java.nio.file.InvalidPathException
import better.files.File
import scala.collection.mutable.ListBuffer
import scalaz.{NotNothing, Validation}


object ReadableColumnsDefaults {
  type ErrorValidation[A] = Validation[String, A]

  implicit object ColumnsReader extends ReadableColumns {
    def readColumnsFromFile(filePath: String): Validation[String, List[List[String]]] = {
      val linesD = readFile(filePath)
      linesD.map(lines => findColumnsFromInputLines(lines, ListBuffer.empty[List[String]]))
    }

    private def readFile(filePath: String): Validation[String, Traversable[String]] = {
      Validation.fromTryCatchThrowable[File, InvalidPathException](File(filePath))
          .bimap(_.toString, _.lines)
    }

    /**
      * The lines with fields "NaN" or "" or "null" are filtered out.
      */
    private def findColumnsFromInputLines(lines: Traversable[String], columns: ListBuffer[List[String]]): List[List[String]] = {
      lines.foldLeft(columns)((acc, str) => {
        val columnsInCurrentLine: Array[String] = str.split("\\t")
        val validLine: Boolean = columnsInCurrentLine.forall {
          p =>
            p != "NaN" && !p.isEmpty && p != "\"\"" && p != "null"
        }
        validLine match {
          case false => acc
          case true => acc += columnsInCurrentLine.toList
        }
      }).reverse.toList
    }
  }
}
