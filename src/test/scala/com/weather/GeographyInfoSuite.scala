package com.weather

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import Position.{createPosition=>position}
import BoundaryLimit.{createBoundary=>boundary}

@RunWith(classOf[JUnitRunner])
class GeographyInfoSuite extends FunSuite{
  val validBounds = boundary(position(23,10), position(50,0))

  test("success if the boundary is within the limit and " +
    "the rows and columns are greater than 2."){
    for {
      numRows <- Seq(5, 3, 100)
      numCols <- Seq(3, 4, 100)
    } yield {
      assert(GeographyInfo.create(numRows, numCols, validBounds){_ + _}.isSuccess)
    }
  }
  test("fails if the boundary is within the limit and " +
    "the rows and columns are less than 2."){
    for {
      numRows <- Seq(0, 3, 100)
      numCols <- Seq(2, 3, 100)
    } yield {
      assert(GeographyInfo.create(numRows, numCols, validBounds){_ + _}.isSuccess)
    }
  }

  test("fails if the boundary has no area"){
    val invalidBoundary = boundary(position(25,0), position(25,0))
    assert(GeographyInfo.create(5, 5, invalidBoundary){_ + _}.isSuccess)

  }
}
