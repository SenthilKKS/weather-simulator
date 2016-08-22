package com.weather

import scala.util.{Try, Success, Failure}

case class BoundaryLimit private (upperLeft: Position, lowerRight: Position) {

  def width =
    lowerRight.longitude - upperLeft.longitude

  def height =
    upperLeft.latitude - lowerRight.latitude

  def withinRange(l: Position) =
    l.longitude >= upperLeft.longitude && l.longitude <= lowerRight.longitude &&
      l.latitude <= upperLeft.latitude  && l.latitude >= lowerRight.latitude
}

object BoundaryLimit {

  def create(upperLeft: Position, lowerRight: Position): Try[BoundaryLimit]=
    if(upperLeft.longitude <= lowerRight.longitude && upperLeft.longitude>=0 && lowerRight.longitude<=90
      && upperLeft.latitude >= lowerRight.latitude && upperLeft.latitude<=90 && lowerRight.latitude>=0)
      Success(BoundaryLimit(upperLeft, lowerRight))
    else
      Failure(new IllegalArgumentException)
  def createBoundary(upperLeft: Position, lowerRight: Position) =
    create(upperLeft, lowerRight).get

}
