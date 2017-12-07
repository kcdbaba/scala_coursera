package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal{scala.math.pow(b(), 2) - (4 * a() * c())}

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal{computeDelta(a, b, c)() match {
        case x if x > 0 => Set((-b() - scala.math.sqrt(x)) / 2 * a(),
                               (-b() + scala.math.sqrt(x)) / 2 * a())
        case x if x == 0 => Set(-b() / (2 * a()))
        case _ => Set()
      }
    }
}
