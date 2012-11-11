package net.fstaals.tl.model

import de.saring.exerciseviewer.parser.ExerciseParser
import de.saring.exerciseviewer.data._
import de.saring.exerciseviewer.parser.ExerciseParserFactory

import org.joda.time.DateTime
import org.joda.time.Duration
import org.scala_tools.time.Imports._

object ActivityFile {

  def fromPath(path: String) = {
    val f = ActivityFile(path)
    f.load()
    if (f.hasData) Some(f) else None
  }

}

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

  def start : Option[DateTime]                  =
    activityData map {e => new DateTime(e.getDate())}

  // unforutnately scala has no applicative
  def end   : Option[DateTime]                  =
    start flatMap {s => duration map (s+) }

  def duration: Option[Duration]                =
    trajectory.endPointOption map {tp => Duration.millis(tp.timestamp) }

  def speed : Option[ActivitySpeed]             =
    activityData map {e => ActivitySpeed.fromESpeed(e.getSpeed())}

  def distance : Option[Distance]               =
    activityData map {a => Distance(a.getSpeed().getDistance())}

  def altitude : Option[ActivityAltitude]       =
    activityData map {e => ActivityAltitude.fromEAltitude(e.getAltitude())}

  def cadence : Option[ActivityCadence]         =
    activityData map {e => ActivityCadence.fromECadence(e.getCadence())}

  def temperature : Option[ActivityTemperature] =
    activityData map {e => ActivityTemperature.fromETemperature(e.getTemperature())}

//  def laps() = activityData map {_.getLapList().toList()}

}

case class ActivitySpeed(val avg: Speed, val max: Speed)

object ActivitySpeed {
  def fromESpeed(e : ExerciseSpeed) =
    ActivitySpeed(e.getSpeedAVG(), e.getSpeedMax())
}

case class ActivityAltitude(val avg: Altitude, val min: Altitude, val max: Altitude, val ascent : Altitude)

object ActivityAltitude {
  def fromEAltitude(e: ExerciseAltitude) =
    ActivityAltitude(e.getAltitudeAVG(),
                     e.getAltitudeMin(), e.getAltitudeMax(), e.getAscent())
}

case class ActivityHeartRate(val avg: HeartRate, val max: HeartRate)

case class ActivityCadence(val avg: Cadence, val max: Cadence)

object ActivityCadence {
  def fromECadence(e : ExerciseCadence) =
    ActivityCadence(e.getCadenceAVG(), e.getCadenceMax())
}


case class ActivityTemperature(val avg: Temperature,
                               val min: Temperature, val max: Temperature)

object ActivityTemperature {
  def fromETemperature(e : ExerciseTemperature) =
    ActivityTemperature(e.getTemperatureAVG(),
                        e.getTemperatureMin(),e.getTemperatureMax())
}
