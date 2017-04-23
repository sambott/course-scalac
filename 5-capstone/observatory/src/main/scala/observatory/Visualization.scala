package observatory

import com.sksamuel.scrimage.{Image, Pixel}

import scala.annotation.tailrec

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  def earthR = 6371

  def greatCircleDistance(pos1: Location, pos2: Location): Double = {
    import Math._
    val deltaLambda = toRadians(abs(pos1.lon - pos2.lon))
    val theta1 = toRadians(pos1.lat)
    val theta2 = toRadians(pos2.lat)
    acos(sin(theta1)*sin(theta2) + cos(theta1) * cos(theta2) * cos(deltaLambda))
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val p = 4
    @tailrec
    def inner(temps: Iterator[(Location, Double)], weightSum: Double, weightedTempSum: Double): Double = {
      if (temps.hasNext) {
        val (point, temp) = temps.next()
        if (point == location) {
          temp
        } else {
          val distance = greatCircleDistance(point, location)
          val weight = 1 / Math.pow(distance, p)
          inner(temps, weightSum + weight, weightedTempSum + (weight * temp))
        }
      }
      else {
        weightedTempSum / weightSum
      }
    }
    inner(temperatures.iterator, 0, 0)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    @tailrec
    def inner(pts: List[(Double, Color)], previousTmp: Double, previousCol: Color): Color = { pts match {
      case Nil => previousCol
      case (nextTmp, nextCol) :: _ if nextTmp == value => nextCol
      case (nextTmp, nextCol) :: _ if nextTmp > value =>
        val tempRatio: Double = (value - previousTmp) / (nextTmp - previousTmp)
        Color(
          Math.round(previousCol.red + (nextCol.red - previousCol.red)*tempRatio).toInt,
          Math.round(previousCol.green + (nextCol.green - previousCol.green)*tempRatio).toInt,
          Math.round(previousCol.blue + (nextCol.blue - previousCol.blue)*tempRatio).toInt
        )
      case (nextTmp, nextCol) :: tail =>
        inner(tail, nextTmp, nextCol)
    }}
    val sortedPoints = points.toList.sortBy(_._1)
    val (firstTmp, firstCol) = sortedPoints.head
    if (firstTmp >= value) firstCol else inner(sortedPoints.tail, firstTmp, firstCol)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val tempList = temperatures.toList
    val colorList = colors.toList.sortBy(_._1)
    val pixels = for {
      latitude <- (-89 to 90).reverse.par
      longitude <- -180 to 179
      temp = predictTemperature(tempList, Location(latitude, longitude))
      colour = interpolateColor(colorList, temp)
    } yield Pixel(colour.red, colour.green, colour.blue, 100)
    Image(360, 180, pixels.toArray)
  }

}

