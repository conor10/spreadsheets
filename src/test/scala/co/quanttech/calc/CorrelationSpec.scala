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

  val audBitcoin = () => ExcelReader.loadNamedRange(fileName, "aud_bitcoin_with_returns")
  val asxSpi200 = () => ExcelReader.loadNamedRange(fileName, "asx_spi200_with_returns")

  val source1 = Source(audBitcoin, "24h Average", "Date")
  val source2 = Source(asxSpi200, "Previous Settlement", "Date")

  val rs1 = source1.load
  val rs2 = source2.load

  val sheet = ExcelReader.loadNamedRange(fileName, "calculations")

  val audBitcoinRet = sheet("Cleaned AUD-Bitcoin Returns").map(_.get.asInstanceOf[Double])
  val asxSpi200Ret = sheet("Cleaned ASX SPI 200 Returns").map(_.get.asInstanceOf[Double])

  def fromSheet(name: String): Any = sheet(name)(0).getOrElse(Double.NaN)

  behavior of "Correlation"

  it should "determine the returns for AUD-BITCOIN prices" in {
    calcReturns(source1.priceRef, rs1).map(x => Option(x)) should equal (rs1("Return"))
  }

  it should "determine the returns for ASX SPI 200 futures index prices" in {
    calcReturns(source2.priceRef, rs2).map(x => Option(x)) should equal (rs2("Return"))
  }

  it should "merge the returns successfully" in {
    val audBitcoin = calcReturns(source1.priceRef, rs1)
    val asxSpi200 = calcReturns(source2.priceRef, rs2)
    val expected = (audBitcoinRet zip asxSpi200Ret)

    val result = merge(source1.dateRef, source2.dateRef, (rs1, audBitcoin), (rs2, asxSpi200))
    result should equal (expected)
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
