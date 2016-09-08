package neuralNetModel

import breeze.linalg.{DenseMatrix, DenseVector}

//   List <Integer> s_l = new ArrayList<Integer>();
//
//  static Double[][][] Thetas;	//first coordinate is 1 indexed
//  static Double[][][] a_l;
//  static Integer[] s_l = new Integer[5];
//
//  static Double[][] h_Theta_all_a4;
//
//  static Double[][][] delta_errors;	//first row irrelevant
//
//  static Double[][][] Deltas;
//
//  static Double[][][] Ds;	// Ds = d/d Theta of J(Theta)


case class RepresentationNeuralNet(Xs: DenseMatrix[Double], Ys: DenseMatrix[Double]) {

  val numOfParameters: Int = Xs.cols
  val trainingSize = Xs.rows
  //size of training set
  val layers = 4
  //two hidden layers
  val K = 1
  //output units
  val lambda = 0.00000001
  val epsilon = Math.pow(10, -4) //possibly set it differently
}

object RepresentationNeuralNet {
  def apply(raws: Array[(Set[Double], Double)]): RepresentationNeuralNet = {


    val X: DenseMatrix[Double] = DenseMatrix.create(raws.size, raws.head._1.size, raws.map(_._1).flatten)
    val Y: DenseMatrix[Double] = DenseMatrix.create(raws.size, 1, raws.map(_._2))
    apply(X, Y)
  }
}
