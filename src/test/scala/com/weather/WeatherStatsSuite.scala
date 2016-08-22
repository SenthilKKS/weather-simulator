package com.weather

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class WeatherStatsSuite extends FunSuite{

   test("addition success") {
     import com.weather.WeatherStats._
     val(stats1,stats2)=(WeatherStats(35,750,40),WeatherStats(-2,950,85))
     val sum = stats1+stats2
     assert(sum.temparature == stats1.temparature+stats2.temparature
       && sum.pressure == stats1.pressure+stats2.pressure
       && sum.humidity == stats1.humidity+stats2.humidity
     )
   }

  test("division success") {
    import com.weather.WeatherStats._
    assert(WeatherStats(99,2250,150)/3 == Some(WeatherStats(33,750,50))
    )
  }
}
