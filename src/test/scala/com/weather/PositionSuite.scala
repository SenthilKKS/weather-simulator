package com.weather

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class PositionSuite extends FunSuite{
  import com.weather.Position._

  test("Test will success provided the latitude range is between " +
    "-90 to 90 and longitude range is between -180 t0 180") {
    assert(create(23,10).isSuccess)
    assert(create(-23,80).isSuccess)
    assert(create(110,-80).isSuccess)
    assert(create(-150,-10).isSuccess)
  }

  test("Test will fail provided the latitude range exceeds " +
    "-90 to 90 and longitude range exceeds -180 t0 180") {
    //intercept[IllegalArgumentException] {
      assert(create(190, 10).isSuccess)
      assert(create(-181,80).isSuccess)
      assert(create(110,-95).isSuccess)
      assert(create(-150,-120).isSuccess)
    //}

  }
}
