package com.weather

import java.text.SimpleDateFormat
import java.util.Date

import com.weather.GeographyInfo.{createGeoInfo => geoInfo}
import com.weather.Position.{createPosition => position}

import scala.annotation.tailrec
import scala.math.{abs, floor, pow}

case class City(name: String, position: Position, elevation: Double)

object WeatherSimulator {
  def main(args: Array[String]) {
    val boundary = BoundaryLimit(position(0, 90), position(90, 0))

    //Loading the image of boundary C1 as specified in Visible Earth Nasa to assess the elevation
    val elevationData = new AltitudeObj("/gebco_08_rev_elev_C1_grey_geo.tiff", boundary)

    //Simulating weather based on boundary and altitude/elevation
    val grid = weatherSimulator(boundary, elevationData)
    println

    //Generating report for each hour (2 * 24) for two days
    val reportHours = 2*24;
    //val gridAfterIterations = generateReports(reportHours, grid, cities(elevationData))
    generateReports(reportHours, grid, cities(elevationData))
    println
    //takeAndAssessPredictions(gridAfterIterations)
  }


  @tailrec
  private def generateReports(reportHours: Int, grid: WeatherInfo, cities: Seq[City]): WeatherInfo =
    if(reportHours > 0) {
      cities.foreach {city =>
        grid.weatherStatsAt(city.position).foreach {stats =>
          println(
            Seq(
              city.name,
              Seq(
                round(city.position.latitude, decimalPlaces = 2).toString,
                round(city.position.longitude, decimalPlaces = 2).toString,
                floor(city.elevation * 255 * 3).toInt.toString
              ).mkString(","),
              convertToISODate(grid.date),
              if(stats.temparature>28) "Sunny" else "Cloudy",
              s"${if(stats.temparature >= 0) "+" else "-"}${round(abs(stats.temparature), 1)}",
              round(stats.pressure, decimalPlaces = 1).toString,
              stats.humidity.toString
            ).mkString("|")
          )
        }
      }
      generateReports(reportHours - 1, grid.afterOneHour, cities)
    } else
      grid

  private def cities(elevationData: AltitudeObj) =
    Seq(
      "Aberdeen" -> position(2, 37),
      "Amsterdam" -> position(4, 52),
      "Ankara" -> position(32, 39),
      "Athens" -> position(23,37),
      "Barcelona" -> position(2, 41),
      "Berlin" -> position(13,52),
      "Birmingham" -> position(1,52),
      "Mumbai" -> position(72,19),
      "Bristol" -> position(2,51),
      "Brussels" -> position(4,50),
      "Budapest" -> position(19,47),
      "Cairo" -> position(31,30),
      "Kolkatta" -> position(88,22),
      "Hamburg" -> position(10,53),
      "Lisbon" -> position(9,38),
      "London" -> position(0,51),
      "New Delhi" -> position(77,28),
      "Paris" -> position(2,48)

    ) map {case (name, loc) =>

      City(name, loc, elevationData.positionAt(loc).get)
    }

  private def weatherSimulator(boundary: BoundaryLimit, elevationData: AltitudeObj) = {

    val rowIndex = 50
    val colIndex = 50
    val temparature = 28
    val pressure = 780
    val humidity = 30
    val grid = geoInfo(rowIndex, colIndex, boundary){(rowIndex, colIndex) =>
      val elevation = {

        val latitude  = rowIndex * (boundary.height / 100) + boundary.lowerRight.latitude
        val longitude = colIndex * (boundary.width / 100) + boundary.upperLeft.longitude
        elevationData.positionAt(position(longitude, latitude)).get
      }

      val stats = WeatherStats(temparature, pressure, humidity).randomized
      WeatherLocation(elevation, stats)
    }

    WeatherInfo(grid, new Date)
  }

  private def round(num: Double, decimalPlaces: Int) =
    math.round(num * pow(10, decimalPlaces)) / pow(10, decimalPlaces)

  private def convertToISODate(date: Date) = {
    new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'") format date
  }

  }
