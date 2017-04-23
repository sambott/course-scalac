package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  test("failingChecker1") {
    val result = Visualization.interpolateColor(List((-1.0,Color(255,0,0)), (0.0,Color(0,0,255))), -0.75)
    assert(result == Color(191,0,64))
  }

  test("temp half way between distance") {
    val distance = Visualization.greatCircleDistance(Location(0,0), Location(0,90))
    assert(distance == Math.PI / 2)
  }

  test("temp half way between") {
    val temps: List[(Location, Double)] = List((Location(0,0), 10.0),(Location(0,90), 20.0))
    val est = Visualization.predictTemperature(temps, Location(0,45))
    assert(est == 15.0)
  }

}
