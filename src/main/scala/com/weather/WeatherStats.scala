package com.weather

import scala.math.round
import scala.util.Random.nextGaussian

case class WeatherStats(temparature: Double, pressure: Double, humidity: Int) {
  def +(other: WeatherStats) =
    WeatherStats(temparature + other.temparature, pressure + other.pressure, humidity + other.humidity)

  def /(n: Int): Option[WeatherStats] =
    if(n == 0)
      None
    else
  Some(WeatherStats(temparature / n, pressure / n, round(humidity.toDouble / n).toInt))

  def randomized = WeatherStats(
    nextGaussian * 3 + temparature ,
    nextGaussian * 10 + pressure,
    round(nextGaussian * 5 + humidity).toInt
  )
 }

