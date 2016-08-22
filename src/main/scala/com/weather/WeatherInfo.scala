package com.weather

import scala.math.{pow, round}

import java.util.{Calendar, Date}

case class WeatherInfo(grid: GeographyInfo[WeatherLocation], date: Date) {

  def weatherStatsAt(location: Position): Option[WeatherStats] = {
    val neighboringWeatherLocations = grid pointsNearBy location
    neighboringWeatherLocations.
      map {_.stats}.
      reduceOption {_ + _}.
      flatMap {sum => sum / neighboringWeatherLocations.length}
  }

  def afterOneHour: WeatherInfo = {
    val newGrid = grid map {(rowIndex, colIndex, point) =>
      point.afterOneHour(
        neighborsStats = grid.neighborsOf(rowIndex, colIndex).map{_.stats}
      ).withRandomizedStats
    }
    copy(grid = newGrid, date = oneHourAfter(date))
  }

  private def oneHourAfter(date: Date): Date = {
    val cal = Calendar.getInstance
    cal setTime date
    cal.add(Calendar.HOUR_OF_DAY, 1)
    cal.getTime
  }


}

trait PressureElevationConstants {
  val MEAN_SEA_LVL_PRESSURE = 1013.25 //As per Wikipedia
  val MEAN_LAND_ELEVATION = 0.25  // Just a guess.
}

case class WeatherLocation(altitude: Double, stats: WeatherStats) extends PressureElevationConstants {
  def afterOneHour(neighborsStats: Seq[WeatherStats]): WeatherLocation = {
    val MAX_ELEVATION_RELATED_WEIGHT_INCREASE = 3
    val MIN_CURRENT_STATE_WEIGHT = 1
    val newPressure =
      if(altitude == 0)
        MEAN_SEA_LVL_PRESSURE
      else
        (neighborsStats.map{_.pressure}.sum + stats.pressure) / (neighborsStats.length + 1)

    // Even a slight change in elevation will have effect in pressure and weight. So elevation
    // near each points will have difference in pressure
    val neighborsWeightedInfluences = neighborsStats collect {
      case WeatherStats(temp, pressure, humidity) if pressure > stats.pressure =>
        val weight = (pressure - stats.pressure) / MEAN_SEA_LVL_PRESSURE * 10
        (weight, temp, humidity)
    }

    val currentStateWeight =
      MIN_CURRENT_STATE_WEIGHT +
        pow(altitude - MEAN_LAND_ELEVATION, 2) * 20 * MAX_ELEVATION_RELATED_WEIGHT_INCREASE

    val currentStateWeightedInfluence = (currentStateWeight, stats.temparature, stats.humidity)
    val weightedInfluences = neighborsWeightedInfluences :+ currentStateWeightedInfluence
    val sumOfWeights = weightedInfluences.map{_._1}.sum
    val weightedAvgTemp =
    weightedInfluences.map{case (w, t, _) => w * t}.sum / sumOfWeights

    val weightedAvgHumidity =
      round(
        weightedInfluences.map{case (w, _, h) => w * h}.sum / sumOfWeights
       ).toInt
    WeatherLocation(altitude, WeatherStats(weightedAvgTemp, newPressure, weightedAvgHumidity))
  }

  def withRandomizedStats = copy(stats = stats.randomized)
}

