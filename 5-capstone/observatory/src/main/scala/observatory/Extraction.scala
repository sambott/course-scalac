package observatory

import java.time.LocalDate

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stationsStream = getClass.getResourceAsStream(stationsFile)
    val stations = for {
      line <- Source.fromInputStream(stationsStream).getLines()
      elmts = line.split(",")
      mapAttempt = Try((elmts(0), elmts(1)) -> Location(elmts(2).toDouble, elmts(3).toDouble))
      if mapAttempt.isSuccess
    } yield mapAttempt.get
    val stationMap = stations.toMap
    stationsStream.close()

    val temperatureStream = getClass.getResourceAsStream(temperaturesFile)
    for {
      line <- Source.fromInputStream(temperatureStream).getLines().toIterable
      elmts = line.split(",")
      if stationMap.contains((elmts(0), elmts(1)))
      tempCelsius = (elmts(4).toDouble - 32) / 1.8
    } yield (LocalDate.of(year, elmts(2).toInt, elmts(3).toInt), stationMap((elmts(0), elmts(1))), tempCelsius)
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    @tailrec
    def inner(rcrds: Iterator[(LocalDate, Location, Double)], state: Map[Location, (Double, Int)]): Iterable[(Location, Double)] = {
      if (rcrds.hasNext) {
        val next = rcrds.next()
        val nextState = state + {
          state.get(next._2) match {
            case Some((sum, count)) => (next._2, (sum + next._3, count + 1))
            case None => (next._2, (next._3, 1))
          }
        }
        inner(rcrds, nextState)
      }
      else {
        state.mapValues(v => v._1 / v._2)
      }
    }
    inner(records.iterator, Map.empty)
  }

}
