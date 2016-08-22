package com.weather

import scala.util.{Try, Success, Failure}

case class Position private (longitude: Double, latitude: Double)

object Position {
  def create(longitude: Double, latitude: Double): Try[Position] =
    if(longitude <= -180 || longitude > 180)
      Failure(new IllegalArgumentException(s"Invalid longitude."))
    else if(latitude < -90 || latitude > 90)
      Failure(new IllegalArgumentException(s"Invalid latitude."))
    else
      Success(Position(longitude, latitude))

  def createPosition(longitude: Double, latitude: Double) =
    create(longitude, latitude).get
  }

