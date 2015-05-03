package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalcSheetSuite extends FunSuite with ShouldMatchers {

  final val MaxTweetLength = 140

  test("eval simple a") {
    val a = new Ref("a")
    val va: Signal[Expr] = Signal(Literal(3))

    val references: Map[String, Signal[Expr]] = Map(("a", va))

    val result = Calculator.eval(a, references)

    assert(result == 3.0)
  }

  test("eval simple a+b") {
    val a = new Ref("a")
    val va: Signal[Expr] = Signal(Literal(3))

    val b = new Ref("b")
    val vb: Signal[Expr] = Signal(Literal(2))

    val aPlusB: Expr = Plus(a, b)

    val references: Map[String, Signal[Expr]] = Map(("a", va), ("b", vb))

    val result = Calculator.eval(aPlusB, references)

    assert(result == 5.0)
  }

  test("eval simple a-b") {
    val a = new Ref("a")
    val va: Signal[Expr] = Signal(Literal(3))

    val b = new Ref("b")
    val vb: Signal[Expr] = Signal(Literal(2))

    val aMinusB: Expr = Minus(a, b)

    val references: Map[String, Signal[Expr]] = Map(("a", va), ("b", vb))

    val result = Calculator.eval(aMinusB, references)

    assert(result == 1.0)
  }

  test("eval simple a*b") {
    val a = new Ref("a")
    val va: Signal[Expr] = Signal(Literal(3))

    val b = new Ref("b")
    val vb: Signal[Expr] = Signal(Literal(2))

    val aTimesB: Expr = Times(a, b)

    val references: Map[String, Signal[Expr]] = Map(("a", va), ("b", vb))

    val result = Calculator.eval(aTimesB, references)

    assert(result == 6.0)
  }
  
  test("eval simple a/b") {
    val a = new Ref("a")
    val va: Signal[Expr] = Signal(Literal(3))

    val b = new Ref("b")
    val vb: Signal[Expr] = Signal(Literal(2))

    val aDivB: Expr = Divide(a, b)

    val references: Map[String, Signal[Expr]] = Map(("a", va), ("b", vb))

    val result = Calculator.eval(aDivB, references)

    assert(result == 1.5)
  }

  test("2*3 = 6") {
    val b = new Ref("b")
    val vb: Signal[Expr] = Signal(Literal(2))
    val a = new Ref("a")
    val va: Signal[Expr] = Signal(Literal(3))

    val vc: Signal[Expr] = Signal(Times(a, b))
    val m: Map[String, Signal[Expr]] = Map(("c", vc), ("a", va), ("b", vb))
    val c = Calculator.computeValues(m)
    assert(c("c").apply() equals 6.0)
  }

}
