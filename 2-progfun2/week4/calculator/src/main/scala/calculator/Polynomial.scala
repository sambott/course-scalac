package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    //b² - 4ac
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    //(-b ± √Δ) / 2a
    val sqDelta = Signal(Math.sqrt(delta()))
    val minusB = Signal(-b())
    val twoA = Signal(2 * a())
    Signal(
      Set(1, -1) map (i => (minusB() + i * sqDelta()) / twoA())
    )
  }
}
