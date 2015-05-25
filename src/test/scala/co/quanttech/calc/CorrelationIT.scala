package co.quanttech.calc

import co.quanttech.excel.ExcelReader
import org.scalatest.FlatSpec

/**
 * Integration test for the end to end model.
 */
class CorrelationIT extends FlatSpec {

  val expected = -0.004706657597210502

  val source1 = Source(Sources.audBitcoin, "24h Average", "Date")
  val source2 = Source(Sources.asxSpi200, "Previous Settlement", "Date")

  "The correlation model" should s"calculate the correlation as $expected" in {
    assert(Correlation.run(source1, source2) === expected)
  }
}

object Sources {
  private val FileName = "src/test/resources/Correlations.xlsx"

  val audBitcoin = () => ExcelReader.loadNamedRange(FileName, "aud_bitcoin")
  val asxSpi200 = () => ExcelReader.loadNamedRange(FileName, "asx_spi200")
}
