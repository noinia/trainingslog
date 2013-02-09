package net.fstaals.tl.model

import de.saring.exerciseviewer.parser.ExerciseParser
import de.saring.exerciseviewer.data._
import de.saring.exerciseviewer.parser.ExerciseParserFactory

import net.fstaals.tl.model.UnitTypes._
import org.joda.time.DateTime
import org.joda.time.Duration
import org.scala_tools.time.Imports._

object ActivityFile {

  def fromPath(path: String) = {
    val f = ActivityFile(path)
    if (f.load) Some(f) else None
  }

}

case class ActivityFile(val path : String) extends HasSummaryData {

  private var activityData : Option[EVExercise] = None

  var trajec   : Trajectory = _

  // load the data, if something goes wrong simply ignore it.
  def load = {
    try {
      loadData
      true
    } catch {
      case e => false
    }
  }

  def hasData = activityData.isDefined

  // load the data. If something goes wrong, throw an exception.
  def loadData = {
    val parser   = ExerciseParserFactory.getParser(path)
    val rawData  = parser.parseExercise(path)
    activityData = Some(rawData)
    trajec   = Trajectory.fromEVSamples(rawData.getSampleList().toList)
  }

  def start : Option[DateTime]                  =
    activityData map {e => new DateTime(e.getDate())}

  // unforutnately scala has no applicative
  def end   : Option[DateTime]                  =
    start flatMap {s => duration map (s+) }

  def duration: Option[Duration]                =
    trajec.endPointOption map {tp => new Duration(tp.timestamp.intValue) }

  def speed : Option[SpeedSummary]             =
    activityData map {e => SpeedSummary.fromESpeed(e.getSpeed())}

  def distance : Option[Distance]               =
    activityData flatMap {a => Option(a.getSpeed()) map {_.getDistance()}}

  def altitude : Option[AltitudeSummary]       =
    activityData map {e => AltitudeSummary.fromEAltitude(e.getAltitude())}

  def cadence : Option[CadenceSummary]         =
    activityData flatMap {e => Option(e.getCadence())} map CadenceSummary.fromECadence

  def temperature : Option[TemperatureSummary] =
    activityData map {e => TemperatureSummary.fromETemperature(e.getTemperature())}

  def trajectory : Option[TrajectoryLike] = Some(trajec)

//  def laps() = activityData map {_.getLapList().toList()}

  def heartRate : Option[HeartRateSummary] =
    activityData map {e => {
      val avg = e.getHeartRateAVG()
      val max = e.getHeartRateMax()
      HeartRateSummary(avg,max)
    }}

  def power : Option[PowerSummary]= None

}

case class SpeedSummary(val avg: Speed, val max: Speed)

object SpeedSummary {
  def fromESpeed(e : ExerciseSpeed) =
    SpeedSummary(e.getSpeedAVG(), e.getSpeedMax())
}

case class AltitudeSummary(val avg: Altitude, val min: Altitude, val max: Altitude, val gain : Altitude)

object AltitudeSummary {
  def fromEAltitude(e: ExerciseAltitude) =
    AltitudeSummary(e.getAltitudeAVG(),
                     e.getAltitudeMin(), e.getAltitudeMax(),
                     e.getAscent())
}

case class HeartRateSummary(val avg: HeartRate, val max: HeartRate)

case class CadenceSummary(val avg: Cadence, val max: Cadence)

object CadenceSummary {
  def fromECadence(e : ExerciseCadence) =
    CadenceSummary(e.getCadenceAVG(), e.getCadenceMax())
}


case class PowerSummary(val avg: Power, val max: Power)


case class TemperatureSummary(val avg: Temperature,
                               val min: Temperature, val max: Temperature)

object TemperatureSummary {
  def fromETemperature(e : ExerciseTemperature) =
    TemperatureSummary(e.getTemperatureAVG(),
                        e.getTemperatureMin(),e.getTemperatureMax())
}
