package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    val possibilities = {
      for {
        lat <- (-89 to 90).par
        lon <- -180 to 179
      } yield (lat, lon) -> Visualization.predictTemperature(temperatures, Location(lat, lon))
    }.toMap

    (lat, lon) => possibilities((lat, lon))
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    val tempGrids = temperaturess.map(makeGrid)
    (lat, lon) => tempGrids.map(f => f(lat, lon)).sum / tempGrids.size
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A sequence of grids containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    val grid = makeGrid(temperatures)
    (lat: Int, lon: Int) => {
      grid(lat, lon) - normals(lat, lon)
    }
  }

}

