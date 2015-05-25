package co.quanttech.calc

import co.quanttech.excel.ExcelReader

/**
 * Our main application runner.
 */
object Runner extends App {
  val source1 = Source(Sources.audBitcoin, "24h Average", "Date")
  val source2 = Source(Sources.asxSpi200, "Previous Settlement", "Date")

  Correlation.run(source1, source2)
}

object Sources {
  val audBitcoin = () => ExcelReader.loadNamedRange(
    "src/test/resources/Correlations.xlsx", "aud_bitcoin")
  val asxSpi200 = () => ExcelReader.loadNamedRange(
    "src/test/resources/Correlations.xlsx", "asx_spi200")
}
