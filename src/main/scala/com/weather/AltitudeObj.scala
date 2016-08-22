package com.weather

import javax.imageio.ImageIO

import scala.math.round

class AltitudeObj(resourcePath: String, boundary: BoundaryLimit) {
  private val imgStream = ImageIO read getClass.getResourceAsStream(resourcePath)
  def positionAt(location: Position): Option[Double] = {
    if(boundary withinRange location) {
      val imgX = round((location.longitude - boundary.upperLeft.longitude) / boundary.width * (imgStream.getWidth - 1)).toInt
      val imgY = round((boundary.upperLeft.latitude - location.latitude) / boundary.height * (imgStream.getHeight - 1)).toInt
      Some((imgStream.getRGB(imgX, imgY) & 0x000000ff).toDouble / 255)
    } else
      None
    }
 }

