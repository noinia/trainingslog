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
  import Lap.ELap

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

  def startTime = Some(new Duration(0))
  def endTime   = duration

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

  lazy val laps : List[Lap] =
    activityData.toList flatMap {a => mkLaps(a.getLapList().toList)}

  def mkLaps(ls: List[ELap]) : List[Lap] = {
    def mkStart(t: Int) : Option[Duration] = Some(new Duration(t * 100))

    val z = (Lap(Some(0),mkStart(0),mkStart(0), None, Some(0), Some(0),
                None, None, None, None, None, () => None), List[Lap]())

    def f(t : ((Lap,List[Lap]),ELap)) = t match { case ((prev,xs),l)  => {
      val nl = Lap.fromELap(prev.lapNumber map {_+1},
                            prev.endTime, prev.endDistance, l, trajectory)
      (nl,nl::xs)
    }}

    ls.foldLeft(z)({case t => f(t)})._2.reverse
  }

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

  def fromLSpeed(l: LapSpeed) =
    SpeedSummary(l.getSpeedAVG(),math.max(l.getSpeedAVG(),l.getSpeedEnd()))
}

case class AltitudeSummary(val avg: Altitude, val min: Altitude, val max: Altitude, val gain : Altitude)

object AltitudeSummary {
  def fromEAltitude(e: ExerciseAltitude) =
    AltitudeSummary(e.getAltitudeAVG(),
                     e.getAltitudeMin(), e.getAltitudeMax(),
                     e.getAscent())

  def fromLAltitude(l: LapAltitude) = {
    val e = l.getAltitude() ; val a = l.getAscent()
    AltitudeSummary(e,e,e,a)
  }
}

case class HeartRateSummary(val avg: HeartRate, val max: HeartRate)

case class CadenceSummary(val avg: Cadence, val max: Cadence)

object CadenceSummary {
  def fromECadence(e : ExerciseCadence) =
    CadenceSummary(e.getCadenceAVG(), e.getCadenceMax())

  def fromLSpeed(l: LapSpeed) = { val c = l.getCadence() ; CadenceSummary(c,c) }
}


case class PowerSummary(val avg: Power, val max: Power)


case class TemperatureSummary(val avg: Temperature,
                               val min: Temperature, val max: Temperature)

object TemperatureSummary {
  def fromETemperature(e : ExerciseTemperature) =
    TemperatureSummary(e.getTemperatureAVG(),
                        e.getTemperatureMin(),e.getTemperatureMax())

  def fromLTemperature(l: LapTemperature) = {
    val t = l.getTemperature()
    TemperatureSummary(t,t,t)
  }

}

case class Lap( val lapNumber     : Option[Int]
              , val startTime     : Option[Duration]
              , val endTime       : Option[Duration]
              , val speed         : Option[SpeedSummary]
              , val startDistance : Option[Distance]
              , val endDistance   : Option[Distance]
              , val heartRate     : Option[HeartRateSummary]
              , val altitude      : Option[AltitudeSummary]
              , val cadence       : Option[CadenceSummary]
              , val temperature   : Option[TemperatureSummary]
              , val power         : Option[PowerSummary]
              , val genTrajectory : () => Option[TrajectoryLike]
             ) extends HasSummaryData {

  def duration = (startTime,endTime) match {
    case (Some(s),Some(e)) => Some(e - s)
    case _                 => None
  }

  def distance = (startDistance,endDistance) match {
    case (Some(s),Some(e)) => Some(e - s)
    case _                 => None
  }

  def trajectory = genTrajectory()

}

object Lap {
  type ELap = de.saring.exerciseviewer.data.Lap

  def fromELap(i: Option[Int], start: Option[Duration], startDist: Option[Distance],
               l: ELap, fullTraj: Option[TrajectoryLike]) = {

    val hr = (Option(l.getHeartRateAVG()),Option(l.getHeartRateMax())) match {
      case (Some(a),Some(m)) => Some(HeartRateSummary(a,m))
      case _                 => None
    }

    val dist = Option(l.getSpeed()) map {_.getDistance()}

    lazy val subTrajectory = None // TODO        fullTraj.subTrajectory()

    Lap(i,
        start, Option(new Duration(l.getTimeSplit() * 100)),
        Option(l.getSpeed()) map {SpeedSummary.fromLSpeed(_)},
        startDist, dist,
        hr,
        Option(l.getAltitude()) map {AltitudeSummary.fromLAltitude(_)},
        Option(l.getSpeed()) map {CadenceSummary.fromLSpeed(_)},
        Option(l.getTemperature()) map {TemperatureSummary.fromLTemperature(_)},
        None, // power
        () => subTrajectory
      )
  }
}
