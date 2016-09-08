package neuralNetModel

import breeze.linalg.DenseMatrix

case class RepresentationNeuralNet(Xs: DenseMatrix[Double], Ys: DenseMatrix[Double]) {

  val numOfParameters: Int = Xs.cols
  val trainingSetSize = Xs.rows     //size of training set
  val layers = 4                    //two hidden layers
  val K = 1                         //output units
  val lambda = 0.00000001
  val epsilon = Math.pow(10, -4)
}

object RepresentationNeuralNet {
  def apply(raws: Array[(List[Double], Double)]): RepresentationNeuralNet = {
    require(raws.size > 0)
    val X: DenseMatrix[Double] = DenseMatrix.create(raws.size, raws.head._1.size, raws.map(_._1).flatten)
    val Y: DenseMatrix[Double] = DenseMatrix.create(raws.size, 1, raws.map(_._2))
    apply(X, Y)
  }
}
