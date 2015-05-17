package co.quanttech.calc

import co.quanttech.calc.Correlation._
import co.quanttech.calc.Stats._
import co.quanttech.excel.ExcelReader
import org.scalatest.{FlatSpec, Matchers}

/**
 * Verification tests using our original sheet.
 */
class CorrelationSpec extends FlatSpec with Matchers {
  val fileName = "src/test/resources/Correlations.xlsx"

  val audBitcoin = ExcelReader.loadNamedRange(_: String, "aud_bitcoin_with_returns")
  val asxSpi200 = ExcelReader.loadNamedRange(_: String, "asx_spi200_with_returns")
  val calculations = ExcelReader.loadNamedRange(_: String, "calculations")

  val source1 = Source(audBitcoin, "24h Average", "Date")
  val source2 = Source(asxSpi200, "Previous Settlement", "Date")

  val rs1 = source1.load(fileName)
  val rs2 = source2.load(fileName)
  val sheet = calculations(fileName)

  val audBitcoinRet = sheet("Cleaned AUD-Bitcoin Returns").map(_.get.asInstanceOf[Double])
  val asxSpi200Ret = sheet("Cleaned ASX SPI 200 Returns").map(_.get.asInstanceOf[Double])

//  val audBitcoinRet = calcReturns(source1.priceRef, rs1)
//  val asxSpi200Ret = calcReturns(source2.priceRef, rs2)

  def fromSheet(name: String): Any = sheet(name)(0).getOrElse(Double.NaN)

  behavior of "Correlation"

  // TODO: Clean up usage of map functions
  it should "determine the returns for AUD-BITCOIN prices" in {
    calcReturns(source1.priceRef, rs1).map(x => Option(x)) should equal (rs1("Return").toList)
  }

  it should "determine the returns for ASX SPI 200 futures index prices" in {
    calcReturns(source2.priceRef, rs2).map(x => Option(x)) should equal (rs2("Return").toList)
  }

  it should "determine the mean for AUD Bitcoin returns" in {
    mean(audBitcoinRet) should equal (fromSheet("AUD-Bitcoin Mean"))
  }

  it should "determine the mean for ASX SPI 200 returns" in {
    mean(asxSpi200Ret) should equal (fromSheet("ASX SPI 200 Mean"))
  }

  it should "determine the standard deviation for AUD-Bitcoin returns" in {
    stdDev(audBitcoinRet) should equal (fromSheet("AUD-Bitcoin StdDev"))
  }

  it should "determine the standard deviation for ASX SPI 200 returns" in {
    stdDev(asxSpi200Ret) should equal (fromSheet("ASX SPI 200 StdDev"))
  }

  it should "determine the covariance of the two price series" in {
    covariance(audBitcoinRet, asxSpi200Ret) should equal (fromSheet("Covariance"))
  }

  it should "determine the correlation of the two price series" in {
    correlation(audBitcoinRet, asxSpi200Ret) should equal (fromSheet("Correlation"))
  }

}
