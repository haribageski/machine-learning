package utils.writers

import better.files.File

object Writer {
  def createFile(pathToDir: String) = {     //creates the file only if there isn't already existing one
    File(pathToDir).createChild("sample.txt")
  }
}
