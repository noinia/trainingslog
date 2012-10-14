package net.fstaals.tl.model

import de.saring.exerciseviewer.parser.ExerciseParser
import de.saring.exerciseviewer.data._
import de.saring.exerciseviewer.parser.ExerciseParserFactory

import org.joda.time.DateTime
import org.joda.time.Duration
import org.scala_tools.time.Imports._

case class ActivityFile(val path : String) {

  private var activityData : Option[EVExercise] = None

  var trajectory   : Trajectory = _

  // load the data, if something goes wrong simply ignore it.
  def load() {
    try {
      loadData()
    } catch {
      case e => {} // if something goes wrong ignore it.
    }
  }

  def hasData = activityData.isDefined

  // load the data. If something goes wrong, throw an exception.
  def loadData() = {
    val parser   = ExerciseParserFactory.getParser(path)
    val rawData  = parser.parseExercise(path)
    activityData = Some(rawData)
    trajectory   = Trajectory.fromEVSamples(rawData.getSampleList().toList)
  }

  def start : Option[DateTime]    = activityData map {e => new DateTime(e.getDate())}

  def end   : Option[DateTime]    = start map {
    // this trajectory should not be empty
    val dur = Duration.millis(trajectory.endPoint.timestamp)
    _ + dur
  }

//  def heartRate() = activityData map {_

  def speed : Option[Speed]       = activityData map {e =>
    Speed.fromESpeed(e.getSpeed())}

  def distance : Option[Int]      = activityData map {_.getSpeed().getDistance()}

  def altitude : Option[Altitude] =
    activityData map {e => Altitude.fromEAltitude(e.getAltitude())}

  def cadence : Option[Cadence]   =
    activityData map {e => Cadence.fromECadence(e.getCadence())}

  def temperature : Option[Temperature] =
    activityData map {e => Temperature.fromETemperature(e.getTemperature())}

//  def laps() = activityData map {_.getLapList().toList()}

}

case class Speed(val avg: Double, val max: Double)

object Speed {
  def fromESpeed(e : ExerciseSpeed) = Speed(e.getSpeedAVG(), e.getSpeedMax())
}

case class Altitude(val avg: Int, val min: Int, val max: Int, val ascent : Int)

object Altitude {
  def fromEAltitude(e: ExerciseAltitude) =
    Altitude(e.getAltitudeAVG(),e.getAltitudeMin(), e.getAltitudeMax(), e.getAscent())
}

case class HeartRate(val avg: Short, val max: Short)

case class Cadence(val avg: Short, val max: Short)

object Cadence {
  def fromECadence(e : ExerciseCadence) = Cadence(e.getCadenceAVG(), e.getCadenceMax())
}


case class Temperature(val avg: Short, val min: Short, val max: Short)

object Temperature {
  def fromETemperature(e : ExerciseTemperature) =
    Temperature(e.getTemperatureAVG(),e.getTemperatureMin(),e.getTemperatureMax())
}
