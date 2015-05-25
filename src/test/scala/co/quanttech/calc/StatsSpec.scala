package co.quanttech.calc

import co.quanttech.calc.Stats._
import org.scalatest.{Matchers, FlatSpec}

/**
 * Correlation specification tests.
 */
class StatsSpec extends FlatSpec with Matchers {

  val series1 = List(3.0, 2.0, 4.0, 5.0, 6.0)
  val series2 = List(9.0, 7.0, 12.0, 15.0, 17.0)

  behavior of "Stats"

  it should "calculate the returns of a data series" in {
    returns(series1) should equal (List(0.0, -0.3333333333333333, 1.0, 0.25, 0.2))
  }

  it should "calculate the mean of a data series" in {
    mean(series1) should equal (4.0)
  }

  it should "calculate the standard deviation of a data series" in {
    stdDev(series1) should equal (1.5811388300841898)
  }

  it should "calculate the covariance of two data series" in {
    covariance(series1, series2) should equal (6.5)
  }

  it should "calculate the correlation between two data series" in {
    correlation(series1, series2) should equal (0.9970544855015815)
  }
}
