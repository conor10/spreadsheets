package co.quanttech

import org.scalatest.FunSuite

import co.quanttech.example.Comprehensions._

/**
 * Comprehensions unit tests.
 */
class ComprehensionsTest extends FunSuite {
  test("two different list diff implementations") {
    val x = (1 until 10).toList
    val y = (0 until 5).toList
    val z = (10 until 20).toList

    assert(diffFlatMap(Nil, Nil) === diffForComp(Nil, Nil))
    assert(diffFlatMap(x, x) === diffForComp(x, x))
    assert(diffFlatMap(x, y) === diffForComp(x, y))
  }
}
