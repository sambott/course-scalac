package observatory

import java.io.File
import java.nio.file.Files

object Main extends App {
  println("starting...")

  val stationsPath: String = "/stations.csv"

  def locationTemperatures(year: Int) = Extraction.locateTemperatures(year, stationsPath, s"/$year.csv")
  def locationAverages(year: Int) = Extraction.locationYearlyAverageRecords(locationTemperatures(year))

  val palette = List(
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0))
  )

  val devPalette = List(
    (7.0, Color(0, 0, 0)),
    (4.0, Color(255, 0, 0)),
    (2.0, Color(255, 255, 0)),
    (0.0, Color(255, 255, 255)),
    (-2.0, Color(0, 255, 255)),
    (-7.0, Color(0, 0, 255))
  )

  println("loading data...")
  val data = {
    for {
      year <- (1975 to 2015).reverse
    } yield year -> locationAverages(year)
  }.toMap

  println("calculating normality")
  val normals = Manipulation.average(data.filterKeys(_ < 1990).values)

  def saveImages(year: Int, zoom: Int, x: Int, y: Int, grid:(Int, Int) => Double): Unit = {

    val tempFolder = new File("target").toPath
    val tempDestination = tempFolder.resolve(s"temperatures/$year/$zoom/$x-$y.png")
    Files.createDirectories(tempDestination.getParent)

    println(s"creating temp image $tempDestination")
    val img = Visualization2.visualizeGrid(grid, palette, zoom, x, y)
    img.output(tempDestination)

    if (year >= 1990) {
      val devDestination = tempFolder.resolve(s"deviations/$year/$zoom/$x-$y.png")
      Files.createDirectories(devDestination.getParent)

      println(s"creating deviation image $devDestination")
      def devs = (lat: Int, lon: Int) => {
        grid(lat, lon) - normals(lat, lon)
      }
      val devImg = Visualization2.visualizeGrid(devs, devPalette, zoom, x, y)
      devImg.output(devDestination)
    }
  }

  for {
    (year, temps) <- data.par
    grid = Manipulation.makeGrid(temps)
    zoom <- 0 to 3
    noTiles = (0 until zoom).foldLeft(1){ case (acc, next) => acc * 2 } - 1
    x <- 0 to noTiles
    y <- 0 to noTiles
  } saveImages(year, zoom, x, y, grid)

}
