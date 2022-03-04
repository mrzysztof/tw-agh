package agh.tw.lab6

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.sys.exit
import scala.util.{Failure, Success, Try}

type Matrix = Array[Array[Double]]

object GaussEliminator {

  def main(args: Array[String]): Unit = {
    if(args.length == 0) {
      println("Pass path to the file.")
      exit(1)
    }

    val filename: String = args(0)
    val matrix: Matrix = MatrixParser.parseMatrix(filename)
    val upperTriangularMatrix: Matrix = eliminate(0, matrix)
    val solvedMatrix: Matrix = Solver.solve(upperTriangularMatrix)
    MatrixParser.writeMatrix(solvedMatrix, filename.stripSuffix("in") ++ "out")
  }

  def eliminate(colNumber: Int, matrix: Matrix): Matrix = {
    if(colNumber < matrix.length - 1){
      val factors: Vector[Double] = findFactors(matrix, colNumber)
      val products: Map[(Int, Int), Double] = findProducts(matrix, colNumber, factors)
      val differences: Seq[(Int, Int, Double)] = findDifferences(matrix, colNumber, products)
      val updatedMatrix: Matrix = updateMatrix(matrix, differences)
      eliminate(colNumber + 1, updatedMatrix)
    }
    else matrix
  }

  /**
   * @return factors - vector of factors required to eliminate corresponding rows
   *        factors = [A_{colNumber,colNumber+1:n}]
   */
  def findFactors(matrix: Matrix, colNumber: Int): Vector[Double] = {
    def findFactor(matrix: Matrix, i: Int, k: Int): Future[Double] =
      Future { matrix(k)(i) / matrix(i)(i) }

    val futureFactors = for {
      k <- (colNumber + 1) until matrix.length
    } yield findFactor(matrix, colNumber, k)

    val factors = Future.sequence(futureFactors.toVector)
    Await.result(factors, Duration.Inf)
  }

  /**
   * @param factors - [A_{colNumber,colNumber+1:n}]
   * @return sequence of tuples (j, k, B_{colNumber,j,k}}
   */
  def findProducts(matrix: Matrix, colNumber: Int,
                   factors: Vector[Double]): Map[(Int, Int), Double] = {
    def findProduct(matrix: Matrix, factors: Vector[Double],
                    i:Int, j: Int, k: Int): Future[((Int, Int), Double)] = Future {
      val factor: Double = factors(k-i-1)
      ((j, k), matrix(i)(j) * factor)
    }

    val futureProducts = for {
      j <- colNumber until (matrix.length + 1)
      k <- (colNumber + 1) until matrix.length
    } yield findProduct(matrix, factors, colNumber, j, k)

    val products = Future.sequence(futureProducts)
    val calculatedProducts = Await.result(products, Duration.Inf)
    calculatedProducts.toMap
  }

  def findDifferences(matrix: Matrix, colNumber: Int,
                      products: Map[(Int, Int), Double]): Seq[(Int, Int, Double)] = {

    def findDifference(matrix: Matrix, products: Map[(Int, Int), Double],
                       i:Int, j: Int, k: Int): Future[(Int, Int, Double)] =
      Future { (j, k, matrix(k)(j) - products((j, k))) }

    val futureDifferences = for {
      j <- colNumber until (matrix.length + 1)
      k <- (colNumber + 1) until matrix.length
    } yield findDifference(matrix, products, colNumber, j, k)

    val differences = Future.sequence(futureDifferences)
    val calculatedDifferences = Await.result(differences, Duration.Inf)
    calculatedDifferences
  }

  def updateMatrix(matrix: Matrix, differences: Seq[(Int, Int, Double)]): Matrix =
    var updatedMatrix: Matrix = matrix.map(_.clone())
    for {
      (j, k, difference) <- differences
    } updatedMatrix(k).update(j, difference)
    updatedMatrix
}
