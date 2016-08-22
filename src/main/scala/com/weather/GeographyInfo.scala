package com.weather

import scala.math.{ceil, floor}
import scala.util.{Try, Success, Failure}


case class GeographyInfo[A] private (points: Seq[Seq[A]], boundary: BoundaryLimit) {
  def map(f: (Int, Int, A) => A): GeographyInfo[A] = {
    val mappedPoints = points.zipWithIndex.map {case (row, rowIndex) =>
      row.zipWithIndex.map {case (point, colIndex) =>
        f(rowIndex, colIndex, point)
      }
    }

    copy(points = mappedPoints)
  }

  def neighborsOf(row: Int, col: Int): Seq[A] =
    if(row >= 0 && row < numRows && col >= 0 && col < numColumns)
      Seq(
        (row - 1, col - 1),
        (row    , col - 1),
        (row + 1, col - 1),
        (row - 1, col + 1),
        (row    , col + 1),
        (row + 1, col + 1),
        (row + 1, col),
        (row - 1, col)
      ) collect {
          case (row, col)
            if
              row >= 0 && row < numRows &&
              col >= 0 && col < numColumns =>
                points(row)(col)
      }
  else
  Seq.empty[A]

  def pointsNearBy(location: Position): Seq[A] =
    if(boundary withinRange location) {
      val colFraction =
        (location.longitude - boundary.upperLeft.longitude) / boundary.width * (numColumns - 1)

      val rowFraction =
        (location.latitude - boundary.lowerRight.latitude) / boundary.height * (numRows - 1)

      Set(
        floor(rowFraction) -> floor(colFraction),
        floor(rowFraction) -> ceil(colFraction),
        ceil(rowFraction)  -> floor(colFraction),
        ceil(rowFraction)  -> ceil(colFraction)
      ).toSeq map {case (row, col) =>
        points(row.toInt)(col.toInt)
      }
    } else
       Seq.empty[A]

  def numRows = points.length
  def numColumns = points.head.length
}

object GeographyInfo {
  def create[A](numRows: Int, numCols: Int, boundary: BoundaryLimit)
    (f: (Int, Int) => A): Try[GeographyInfo[A]] = {
      if(numRows < 2 || numCols < 2 || boundary.width == 0 || boundary.height == 0)
        Failure(new IllegalArgumentException)
      else {
      Success(GeographyInfo(Seq.tabulate(numRows, numCols)(f), boundary))
    }
  }

  def createGeoInfo[A](numRows: Int, numCols: Int, boundary: BoundaryLimit)
    (f: (Int, Int) => A): GeographyInfo[A] =
      create(numRows, numCols, boundary)(f).get
  }

