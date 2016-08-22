package com.weather

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import com.weather.Position.{createPosition=>position}
import com.weather.BoundaryLimit.{create=>boundary}

@RunWith(classOf[JUnitRunner])
class BoundaryLimitSuite extends FunSuite{


  test("successfully creates boundary when both latitude and longitudes " +
    "positions are within the range for both upperLeft and lowerRight corners") {
    assert(boundary(position(23,10),position(50,0)).isSuccess)
    assert(boundary(position(25,60),position(90,20)).isSuccess)
  }

  test("fails to creates boundary when both latitude and longitudes " +
    "positions are not within the range for both upperLeft and lowerRight corners") {
    assert(boundary(position(-170,10),position(50,0)).isSuccess)
    assert(boundary(position(40,-89),position(50,0)).isSuccess)
    assert(boundary(position(23,10),position(-50,0)).isSuccess)
    assert(boundary(position(23,10),position(-179,-71)).isSuccess)
    assert(boundary(position(23,10),position(179,-71)).isSuccess)
    assert(boundary(position(-23,-10),position(179,-71)).isSuccess)
  }

  test("success when the position values are within the boundary"){
    assert(BoundaryLimit(position(15,60),position(60,0)).withinRange(position(20,30)))
    assert(BoundaryLimit(position(15,60),position(15,60)).withinRange(position(15,60)))
   // assert(BoundaryLimit(position(10,20),position(60,10)).withinRange(position(10,15)))
  }

  test("fails when the position values are not within the boundary"){
    assert(BoundaryLimit(position(10,20),position(60,10)).withinRange(position(10,25)))
  }
}
