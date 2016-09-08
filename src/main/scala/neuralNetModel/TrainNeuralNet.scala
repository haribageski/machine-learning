package neuralNetModel

import breeze.linalg.{*, Axis, DenseMatrix, DenseVector}

import scala.collection.immutable.{IndexedSeq, TreeSet}
import scalaz._
import scalaz.Scalaz._

case class TrainNeuralNet(representationNeuralNet: RepresentationNeuralNet) {
  /**
    * Xs is expected to be of dimension numOfParameters x numOfSamples.
    * The returning Matrix is of dimension K x numOfSamples.
    */

  def applyNeuralNetOnGivenSample(Xs: DenseMatrix[Double], thetas: List[DenseMatrix[Double]]): DenseMatrix[Double] = {
    forwardPropagation(Xs, thetas).last
  }

  /**
    * The core functionality, this is the top level , it calls all the other functions.
    */
  def learnTheta(): List[DenseMatrix[Double]] = {

    val thetas_Deltas: List[(DenseMatrix[Double], DenseMatrix[Double])] =
    initializeThetaAndDelta(representationNeuralNet.trainingSetSize, representationNeuralNet.numOfParameters)
    val thetas: List[DenseMatrix[Double]] = thetas_Deltas.map(_._1)
    val deltas: List[DenseMatrix[Double]] = thetas_Deltas.map(_._2)

    @scala.annotation.tailrec
    def iterate(thetas: List[DenseMatrix[Double]], deltas: List[DenseMatrix[Double]]): List[DenseMatrix[Double]] = {
      val a_l: List[DenseMatrix[Double]] = forwardPropagation(thetas)
      val Ds: List[DenseMatrix[Double]] = backwardPropagation(a_l, thetas, deltas)
      val updatedTheta: List[DenseMatrix[Double]] = updateTheta(thetas, Ds)
      val cost = costFunctionJtheta(a_l.last, updatedTheta)
      println("Cost J_theta:" + cost)
      if (cost > 0)
      iterate(updatedTheta, Ds)
      else
      updatedTheta
    }
    iterate(thetas, deltas)
  }

  def initializeThetaAndDelta(trainingSize: Int, numOfParameters: Int): List[(DenseMatrix[Double], DenseMatrix[Double])] = {
    val s_l: Vector[Int] = Vector.fill(4)(numOfParameters) :+ (1)

    val theta3 = DenseMatrix.fill(representationNeuralNet.K, numOfParameters + 1)(Math.random() * representationNeuralNet.epsilon - representationNeuralNet.epsilon)
    val delta3 = DenseMatrix.fill(representationNeuralNet.K, numOfParameters + 1)(0d)
    (for {
    l <- 1 until representationNeuralNet.layers - 1
    thetaS = DenseMatrix.fill(numOfParameters, numOfParameters + 1)(Math.random() * representationNeuralNet.epsilon - representationNeuralNet.epsilon)
    deltaS = DenseMatrix.fill(numOfParameters, numOfParameters + 1)(0d)
    } yield (thetaS, deltaS)
    ).toList :+ ((theta3, delta3))
  }


  def forwardPropagation(thetaS: List[DenseMatrix[Double]]): List[DenseMatrix[Double]] = {
    forwardPropagation(
    representationNeuralNet.Xs //dimension: numOfParameters x trainingSize
    , thetaS)
  }


  def forwardPropagation(Xs: DenseMatrix[Double], thetaS: List[DenseMatrix[Double]]): List[DenseMatrix[Double]] = {
    val a1 = DenseMatrix.vertcat(   //dimension: numOfParameters+1 x trainingSize
    DenseMatrix.ones[Double](1, representationNeuralNet.trainingSetSize),
    Xs.t
    )
    val z2: DenseMatrix[Double] = thetaS(0) * a1 //dimension: numOfParameters x trainingSize
    val a2: DenseMatrix[Double] = DenseMatrix.vertcat(//dimension: numOfParameters+1 x trainingSize
    DenseMatrix.ones(1, representationNeuralNet.trainingSetSize),
    z2.map(gFunc)
    )
    val z3: DenseMatrix[Double] = thetaS(1) * a2 //dimension: numOfParameters x trainingSize
    val a3: DenseMatrix[Double] = DenseMatrix.vertcat(//dimension: numOfParameters+1 x trainingSize
    DenseMatrix.ones(1, representationNeuralNet.trainingSetSize),
    z3.map(gFunc)
    )
    val z4: DenseMatrix[Double] = thetaS(2) * a3 //dimension: K x trainingSize
    val a4 = z4.map(gFunc) //also known as h_Theta, dimension: K x trainingSize

    List(a1, a2, a3, a4)
  }


  def backwardPropagation(a_l: List[DenseMatrix[Double]], thetas: List[DenseMatrix[Double]], deltas: List[DenseMatrix[Double]]): List[DenseMatrix[Double]] = {
    val a1 = a_l.head
    val a2 = a_l.tail.head
    val a3 = a_l.tail.tail.head
    val a4 = a_l.tail.tail.tail.head

    val delta_errors_4: DenseMatrix[Double] = a4 - representationNeuralNet.Ys.t //dimension: K x trainingSize
    //param+1 x 1  , 1 x trainingSize => param+1 x trainingSIze
    val delta_errors_3: DenseMatrix[Double] = //param x trainingSize
    ((thetas(2).t * delta_errors_4) :*
    (a3 :* a3.map(1 - _)))
      .delete(0, Axis._0) //delete first row
    val delta_errors_2: DenseMatrix[Double] =   //param x trainingSize
    ((thetas(1).t * delta_errors_3) :* (a2 :* a2.map(1 - _)))
      .delete(0, Axis._0) //delete first row
    val deltaErrors: List[DenseMatrix[Double]] = List(delta_errors_2, delta_errors_3, delta_errors_4)
    //numOfParameters x numOfParameters+1,    param x trainingSize *  trainingSize x numOfParameters+1
    val newDeltas: List[DenseMatrix[Double]] = (deltas zip deltaErrors zip (a_l.take(representationNeuralNet.layers - 1))).map{
      delta_deltaError_a =>
        val delta = delta_deltaError_a._1._1
        val deltaError = delta_deltaError_a._1._2
        val a = delta_deltaError_a._2

        delta :+
        (deltaError * (a.t))
    }

    val Ds: List[DenseMatrix[Double]] = newDeltas.map(_.map(_ / representationNeuralNet.trainingSetSize))

    val adjustedDs: List[DenseMatrix[Double]] = (Ds zip thetas).map {
      ds_thetas => {
        val ds = ds_thetas._1
        val theta = ds_thetas._2
        //TODO: Check if theta or Ds should be thetas.tail
        for {
        row <- 0 until theta.rows - 1
        col <- 0 until theta.cols - 1
        } if (col > 0)
        ds.update(row, col, ds(row, col) +
        (theta(row, col) * representationNeuralNet.lambda))
        ds
      }
    }
    adjustedDs
  }

  def updateTheta(thetas: List[DenseMatrix[Double]], Ds: List[DenseMatrix[Double]]): List[DenseMatrix[Double]] = {
    (Ds zip thetas).map {
      (ds_thetas: (DenseMatrix[Double], DenseMatrix[Double])) => {
        val ds = ds_thetas._1
        val theta = ds_thetas._2
        //TODO: Check if theta or Ds should be thetas.tail
        theta - ds.map(_ * representationNeuralNet.lambda)
      }
    }
  }

  def costFunctionJtheta(hTheta: DenseMatrix[Double], thetas: List[DenseMatrix[Double]]) = {
    val Yt = representationNeuralNet.Ys.t

    val firstSum = (Yt.data.toList |@| hTheta.data.toList) {
      (y, hThetaElement) => y * Math.log(hThetaElement) + (1 - y) * Math.log(1 - hThetaElement)
    }.fold(0d)(_ + _) / (-representationNeuralNet.trainingSetSize)

    val secondSum = thetas.map(theta => theta.data.fold(0d)(Math.pow(_, 2) + Math.pow(_, 2)))
      .fold(0d)(_ + _)

    firstSum + representationNeuralNet.lambda * secondSum / (2 * representationNeuralNet.trainingSetSize)
  }

  def gFunc(z: Double) = {
    1 / (1 + Math.pow(Math.E, -z))
  }

  def testSetError(testSet: RepresentationNeuralNet, thetaS: List[DenseMatrix[Double]]): Double = {
    val X: DenseMatrix[Double] = testSet.Xs
    val Y = testSet.Ys
    val h_Theta = forwardPropagation(X, thetaS).last    //dimension: 1 x testSize
//    (Y :* h_Theta.t.map(Math.log(_)) + Y.map(1 - _) :* h_Theta.t.map(h => Math.log(1 - h)))   //TODO: Decide what the cost function of the test error should be.
    ((Y - h_Theta.t).map(Math.pow(_, 2d)))
      .data.fold(0d)(_ + _) / (-testSet.trainingSetSize)
  }

}
