package com.weather

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class WeatherInfoSuite extends FunSuite with PressureElevationConstants{

  test("success when there is no elevation from sea level " +
    "then pressure will be same as avg sea level pressure"){
    assert(WeatherLocation(altitude = 0, WeatherStats(32, 1025, 55))
      .afterOneHour(Seq[WeatherStats]())
      .stats.pressure == MEAN_SEA_LVL_PRESSURE)
  }
  test("success when the mean of neighbours pressure and current location"){

    val neighborsPressures = Seq(850, 1200, 950)
    val neighborsStats = neighborsPressures map {WeatherStats(35, _, 90)}
    val thisPoint = WeatherLocation(0.5, WeatherStats(35, pressure = 1000, 90))
    assert(thisPoint.afterOneHour(neighborsStats).stats.pressure ==
      (neighborsPressures.sum + thisPoint.stats.pressure) /
        (neighborsPressures.length + 1))

  }

}
