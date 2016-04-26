//package utils.readers
//
//import ReadableDefaults._
//
//object ReadSyntax {
//  implicit class ReadOps[A](value: A) {
//    def readParameter(implicit readableParameter: ReadableParameter[A], filePath: String, symbol: String,
//                      indexOfValue: Int): A = {
//      readableParameter.readParameterFromFile(filePath, symbol, indexOfValue)
//    }
//  }
//}
