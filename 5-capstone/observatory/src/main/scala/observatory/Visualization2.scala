package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    x: Double,
    y: Double,
    d00: Double,
    d01: Double,
    d10: Double,
    d11: Double
  ): Double = {
    d00 * (1 - x) * (1 - y) + d10 * x * (1 - y) + d01 * (1 - x) * y + d11 * x * y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {
    val imageWidth = 256
    val imageHeight = 256
    val alpha = 127

    val locations = for {
      i <- (0 until imageWidth).par
      j <- 0 until imageHeight
      xTile = i.toDouble / imageWidth + x
      yTile = j.toDouble / imageHeight + y
    } yield j * imageWidth + i -> Location(Interaction.yToLat(yTile, zoom), Interaction.xToLon(xTile, zoom))

    val pixels = locations.map { case (i, loc) =>

      val latRange = List(Math.floor(loc.lat).toInt, Math.ceil(loc.lat).toInt)
      val lonRange = List(Math.floor(loc.lon).toInt, Math.ceil(loc.lon).toInt)

      val d = {
        for {
          xPos <- 0 to 1
          yPos <- 0 to 1
        } yield (xPos, yPos) -> grid(latRange(1 - yPos), lonRange(xPos))
      }.toMap

      val dx = loc.lon - lonRange.head
      val dy = latRange(1) - loc.lat

      val temp = bilinearInterpolation(dx, dy, d((0, 0)), d((0, 1)), d((1, 0)), d((1, 1)))
      val colour = Visualization.interpolateColor(colors, temp)
      (i, Pixel(colour.red, colour.green, colour.blue, alpha))
    }

    Image(imageWidth, imageHeight, pixels.toArray.sortBy(_._1).map(_._2))
  }

}
