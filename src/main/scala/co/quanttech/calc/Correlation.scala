package co.quanttech.calc

import java.util.Date

import co.quanttech.excel.{ResultSet, ExcelReader}

/**
 * Our Correlation model implementation.
 */
object Correlation {

  def run(s1: Source, s2: Source, fileName: String): Double = {
    val rs1 = s1.load(fileName)
    val rs2 = s2.load(fileName)

    val s1Returns = calcReturns(s1.priceRef, rs1)
    val s2Returns = calcReturns(s2.priceRef, rs2)

    val mergedRet = merge(s1.dateRef, s2.dateRef, (rs1, s1Returns), (rs2, s2Returns))

    calcCorrelation(mergedRet)
  }

  def calcReturns(attr: String, rs: ResultSet): List[Double] = {
    val result =
      for {
        (entry, idx) <- rs.zipWithIndex
        if idx != 0
        currPrice = entry(attr).getOrElse(Double.NaN).asInstanceOf[Double]
        prevPrice = rs(idx - 1)(attr).getOrElse(Double.NaN).asInstanceOf[Double]
      }
      yield (currPrice - prevPrice) / prevPrice
    0.0 :: result.toList
  }

  def calcCorrelation(returns: Iterable[(Double, Double)]): Double = {
    val x = returns.map(_._1)
    val y = returns.map(_._2)

    Stats.correlation(x, y)
  }

  def merge(attr1: String, attr2: String, ds1: (ResultSet, List[Double]),
            ds2: (ResultSet, List[Double])): Iterable[(Double, Double)] = {

    val keys = (ds1._1(attr1) ++ ds2._1(attr2)).distinct.sortWith {
      case (d1: Option[Date], d2: Option[Date]) => d1.get.getTime < d2.get.getTime
    }

    val map1 = (ds1._1(attr1) zip ds1._2).toMap
    val map2 = (ds2._1(attr2) zip ds2._2).toMap

    for {
      k <- keys
      v1 = map1.getOrElse(k, 0.0)
      v2 = map2.getOrElse(k, 0.0)
    }
    yield {
      if (v1 != 0.0 && v2 != 0.0) (v1, v2)
      else (0.0, 0.0)
    }
  }
}

case class Source(source: String => ResultSet, priceRef: String, dateRef: String) {
  def load(sourceFile: String): ResultSet = source(sourceFile)
}

object Runner extends App {
  val source1 = Source(Sources.audBitcoin, "24h Average", "Date")
  val source2 = Source(Sources.asxSpi200, "Previous Settlement", "Date")

  Correlation.run(source1, source2, "src/test/resources/Correlations.xlsx")
}

object Sources {
  val audBitcoin = ExcelReader.loadNamedRange(_: String, "aud_bitcoin")
  val asxSpi200 = ExcelReader.loadNamedRange(_: String, "asx_spi200")
}




