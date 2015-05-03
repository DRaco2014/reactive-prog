package calculator
import scala.math.pow
import scala.math.sqrt

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal(pow(b(), 2) - 4 * a() * c())
  }

  /*def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val d = delta()

    if (d < 0) {
      Signal(Set())
    } else if (d == 0) {
      val x1 = (-b()) / 2 * a()
      Signal(Set(x1))
    } else {
      val x1 = (-b() + sqrt(d)) / 2 * a()
      val x2 = (-b() - sqrt(d)) / 2 * a()
      Signal(Set(x1, x2))
    }
  }*/
  
  
  def computeSolutions(a: Signal[Double], b: Signal[Double], 
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
  Signal({
    val ddd = delta()
    ddd match {
      case 0 => Set((-1 * b()) / (2 * a()))
      case d if (d > 0) => Set(
        (-1 * b() - sqrt(ddd)) / (2 * a()),
        (-1 * b() + sqrt(ddd)) / (2 * a())
      )
      case _ => {
        Set()
      }
    }
  })
}
}
