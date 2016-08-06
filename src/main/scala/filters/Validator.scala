package filters

object Validator {

  //TODO: Is this a good place for Validator? It is not really a filter.
  implicit object Validator {
    def validateValueInLines(indexes: Seq[Int])(line: List[String]): Boolean = {
      indexes.forall(index => line(index).toDouble != Double.NaN && line(index) != null)
    }
  }
}
