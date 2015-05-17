package co.quanttech.calc

/**
 * Statistical functions library.
 */
object Stats {
  def mean(x: Iterable[Double]): Double = x.sum / x.size

  def stdDev(x: Iterable[Double]): Double = stdDev(x, mean(x))

  def stdDev(x: Iterable[Double], mean: Double): Double =
    Math.sqrt(
      x.fold(0.0) ((acc, r) => acc + (r - mean) * (r - mean)) / (x.size - 1)
    )

  def covariance(x: Iterable[Double], y: Iterable[Double]): Double =
    covariance(x zip y, mean(x), mean(y))

  def covariance(xy: Iterable[(Double, Double)], muX: Double, muY: Double): Double =
    xy.foldLeft(0.0)((acc, xy) => acc + (xy._1 - muX) * (xy._2 - muY)) / (xy.size - 1)

  def correlation(x: Iterable[Double], y: Iterable[Double]): Double =
    covariance(x, y) / (stdDev(x) * stdDev(y))
}
