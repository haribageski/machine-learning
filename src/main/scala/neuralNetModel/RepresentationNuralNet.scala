package neuralNetModel


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


case class RepresentationNuralNet(Xs: Set[List[Double]], Ys: Set[Double]) {
  val numOfParameters: Int = Xs.head.size
  val trainingSize = Xs.size		            //size of training set
  val layers = 4		              //two hidden layers
  val K = 1                       //output units
  val lambda = 0.00000001
  val emsilon = Math.pow(10, -4)	//possibly set it differently
}

object RepresentationNuralNet {
  def apply(raws: Set[(List[Double], Double)]) = apply(raws.map(_._1), raws.map(_._2))
}
