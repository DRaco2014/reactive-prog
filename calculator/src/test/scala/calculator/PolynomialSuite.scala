package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class PolynomialSuite extends FunSuite with ShouldMatchers {

  /**
   * ****************
   * * TWEET LENGTH **
   * ****************
   */

  test("delta > 0") {
    val a = new Var(1.0)
    val b = new Var(1.0)
    val c = new Var(-2.0)

    val delta = Polynomial.computeDelta(a, b, c)
    assert(delta() == 9)
    val solutions = Polynomial.computeSolutions(a, b, c, delta)
    assert(solutions() == Set(1.0, -2.0))
  }

  test("delta == 0") {
    val a = new Var(1.0)
    val b = new Var(2.0)
    val c = new Var(1.0)

    val delta = Polynomial.computeDelta(a, b, c)
    assert(delta() == 0)
    val solutions = Polynomial.computeSolutions(a, b, c, delta)
    assert(solutions() == Set(-1.0))

  }

  test("delta < 0") {
    val a = new Var(1.0)
    val b = new Var(1.0)
    val c = new Var(1.0)

    val delta = Polynomial.computeDelta(a, b, c)
    assert(delta() == -3)
    val solutions = Polynomial.computeSolutions(a, b, c, delta)
    assert(solutions() == Set())

  }

}
