package neuralNetModel

import breeze.linalg.DenseMatrix

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


case class RepresentationNuralNet(Xs: DenseMatrix[Set[Double]], Ys: DenseMatrix[Double]) {

  val numOfParameters: Int = Xs.cols
  val trainingSize = Xs.rows
  //size of training set
  val layers = 4
  //two hidden layers
  val K = 1
  //output units
  val lambda = 0.00000001
  val emsilon = Math.pow(10, -4) //possibly set it differently
}

object RepresentationNuralNet {
  def apply(raws: Set[(List[Double], Double)]): RepresentationNuralNet = {
    val X: DenseMatrix[Set[Double]] = DenseMatrix.create(raws.size, raws.head._1.size, raws.toList.map(_._1.toSet).toArray)
    val Y: DenseMatrix[Double] = DenseMatrix.create(raws.size, 1, raws.toList.map(_._2).toArray)
    apply(X, Y)
  }
}
