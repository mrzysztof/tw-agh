package agh.tw.lab6

import scala.sys.exit
import scala.util.{Failure, Success, Try}
import java.io.{FileWriter, File}

object MatrixParser{
  def parseMatrix(filename: String): Array[Array[Double]] =
    val lines: Seq[String] = readMatrix(filename)
    val n: Int = lines(0).toInt
    val matrixRows: Array[Array[Double]] =
      lines.slice(1, n+1).map(r => r.split("\\s").map(_.toDouble)).toArray
    val rhsVector: Array[Double] = lines(n+1).split("\\s").map(_.toDouble)

    for{
      (row, rhsElement) <- matrixRows.zip(rhsVector)
    } yield (row :+ rhsElement)


  def readMatrix(filename: String): Seq[String] =
    Try {
      val bufferedSource = scala.io.Source.fromFile(filename)
      bufferedSource
    } match {
      case Failure(message)  => {
        println(message)
        exit(1)
      }
      case Success(bufferedSource) => {
        val lines = bufferedSource.getLines().toSeq
        bufferedSource.close()
        lines
      }
    }

  def writeMatrix(matrix: Array[Array[Double]], filename: String): Unit =
    val outputStream = new FileWriter(new File(filename))
    val rhsVector: Array[Double] = matrix.map(r => r.last)
    val coefficients: Array[Array[Double]] = matrix.map(r => r.dropRight(1))

    outputStream.write(matrix.length.toString + '\n')
    for (row <- coefficients) {
      outputStream.write(row.mkString(" "))
      outputStream.write('\n')
    }
    outputStream.write(rhsVector.mkString(" "))
    outputStream.close()
}
