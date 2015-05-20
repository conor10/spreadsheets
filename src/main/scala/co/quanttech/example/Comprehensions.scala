package co.quanttech.example

/**
 * Examples used in comprehensions slide.
 */
object Comprehensions {

  def diffFlatMap(xs: List[_], ys: List[_]): List[(_, _)] =
    xs.flatMap {
      x => ys.withFilter(y => y != x).map(y => (x, y))
    }

  // Using a for-comprehension
  def diffForComp(xs: List[_], ys: List[_]): List[(_, _)] =
    for {
      x <- xs
      y <- ys
      if x != y
    } yield (x, y)
}

