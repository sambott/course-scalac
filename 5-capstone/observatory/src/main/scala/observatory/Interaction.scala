package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  def xToLon(x: Double, z: Int): Double = 360*x/Math.pow(2, z) - 180
  def yToLat(y: Double, z: Int) : Double = Math.atan(Math.sinh(Math.PI - y * 2 * Math.PI / Math.pow(2, z))) * 180 / Math.PI

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val lon = xToLon(x, zoom)
    val lat = yToLat(y, zoom)
    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val imageWidth = 256
    val imageHeight = 256
    val alpha = 127
    val pixels = for {
      i <- (0 until imageWidth).par
      j <- 0 until imageHeight
      xTile = i.toDouble / imageWidth + x
      yTile = j.toDouble / imageHeight + y
      temp = Visualization.predictTemperature(temperatures, Location(yToLat(yTile, zoom), xToLon(xTile, zoom)))
      colour = Visualization.interpolateColor(colors, temp)
    } yield j * imageWidth + i -> Pixel(colour.red, colour.green, colour.blue, alpha)
    Image(imageWidth, imageHeight, pixels.toArray.sortBy(_._1). map(_._2))
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    for {
      zoom <- (0 to 3).par
      (year, data) <- yearlyData.par
      noTiles = (0 until zoom).foldLeft(1){ case (acc, next) => acc * 2 } - 1
      x <- 0 to noTiles
      y <- 0 to noTiles
    } generateImage(year, zoom, x, y, data)
  }

}
