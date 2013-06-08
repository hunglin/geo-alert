package controllers

import play.api._
import play.api.mvc._
import play.Logger

object Application extends Controller {
  
  import java.text.SimpleDateFormat
  import play.api.Play.current
  
  val data = Play.getExistingFile("/resources/CrimeData.txt").get
  val dateParser = new SimpleDateFormat("yyyy-MM-dd")
  
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def getCrimeRecord(month: Int, distance: Int, latitude: Double, longitude: Double) = Action {
    
    case class CrimeRecord(
        crimeType: String,
        date: String,
        time: String,
        address: String,
        zipcode: String,
        description: String,
        lat: Double,
        long: Double) {
      
      val timestamp: Option[Long] = {
        try {
          Some(dateParser.parse(date).getTime())
        }
        catch {
          case ex: Throwable =>
            Logger.warn(s"Cannot parse this date string: $date")
            None
        }
      }
      
      def withIn(timeRange: Int): Boolean = {
        timestamp match {
          case Some(x) =>
            (System.currentTimeMillis() - x) < (timeRange.toLong * 30 * 24 * 60 * 60 * 1000).toLong
          case None =>
            false
        }
      }
      
      def distance(lat2: Double, long2: Double): Double = {
        val dLat = (lat2-lat) * Math.PI / 180
        val dLon = (long2-long) * Math.PI / 180

        val a = Math.sin(dLat/2) * Math.sin(dLat/2) +
        		Math.sin(dLon/2) * Math.sin(dLon/2) * Math.cos(lat * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) 
        val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a)) 
        3959 * c
      }
    }
        
    val records = io.Source.fromFile(data).getLines
    .map(x => x.split("\t"))
    .map(x => CrimeRecord(x(2), x(3), x(4), x(5), x(6), x(11), x(19).toDouble, x(20).toDouble))
    .filter(x => x.distance(latitude, longitude) < distance && x.withIn(month))
    .mkString("\n")
    
    Ok(s"crime record: \n$records")
  }
  
}