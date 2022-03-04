package agh.tw.lab6

import scala.language.postfixOps

object Solver {
  def solve(upperTriangularMatrix: Matrix): Matrix =
    backSubstitute(upperTriangularMatrix, upperTriangularMatrix.length - 1)

  def backSubstitute(matrix: Matrix, colNumber: Int): Matrix =
    if (colNumber >= 0) {
      var newMatrix: Matrix = matrix.map(_.clone())
      val n: Int = matrix.length
      for {
        j <- (colNumber + 1) until n
        x_j = newMatrix(j)(n)
        coeff_j = newMatrix(colNumber)(j)
      } {
        newMatrix(colNumber)(n) -= (coeff_j * x_j)
        newMatrix(colNumber)(j) = 0.0
      }
      newMatrix(colNumber)(n) /= newMatrix(colNumber)(colNumber)
      newMatrix(colNumber)(colNumber) = 1.0

      backSubstitute(newMatrix, colNumber - 1)
    }
    else matrix
}
