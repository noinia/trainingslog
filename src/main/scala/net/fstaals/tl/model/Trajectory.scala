package net.fstaals.tl.model

import scala.collection.SortedMap
import de.saring.exerciseviewer.data._
import net.fstaals.tl.model.UnitTypes._

case class Trajectory(val points : SortedMap[Trajectory.Timestamp,TrajectoryPoint]) {

  def startPoint = startPointOption.get

  def endPoint   = endPointOption.get

  def startPointOption = points.headOption map {_._2}
  def endPointOption   = points.lastOption map {_._2}

}

object Trajectory {
  type Timestamp = Long

  def fromEVSamples(xs : List[ExerciseSample]) =
    Trajectory(xs map {s => {val p = TrajectoryPoint.fromExerciseSample(s)
                             (p.timestamp,p)}})

  def apply(xs : Seq[(Timestamp,TrajectoryPoint)]) =
    new Trajectory(SortedMap.empty[Timestamp,TrajectoryPoint] ++ xs)

}


case class TrajectoryPoint(
    val timestamp   : Long                  // time since start in miliseconds
  , val latitude    : Option[Double]       = None // in degrees
  , val longitude   : Option[Double]       = None // in degrees
  , val heartRate   : Option[HeartRate]    = None // in bpm
  , val altitude    : Option[Altitude]     = None // in meters?
  , val speed       : Option[Speed]        = None // in Km/h
  , val cadence     : Option[Cadence]      = None // in rpm
  , val power       : Option[Power]        = None // in watts ?
  , val temperature : Option[Temperature]  = None // in degrees celcius
  , val distance    : Option[Distance]     = None // since start in meters
)

object TrajectoryPoint {
  def fromExerciseSample(es : ExerciseSample) =
    TrajectoryPoint( es.getTimestamp()
                   , Option(es.getPosition())    map {_.getLatitude() }
                   , Option(es.getPosition())    map {_.getLongitude()}
                   , Option(es.getHeartRate())
                   , Option(es.getAltitude())
                   , Option(es.getSpeed())
                   , Option(es.getCadence())
                   , None // power
                   , Option(es.getTemperature())
                   , Option(es.getDistance())
                   )


}

case class FilteredTrajectory(val pieces : List[Trajectory]) {

}
