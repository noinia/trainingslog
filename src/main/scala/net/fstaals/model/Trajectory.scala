package net.fstaals.model

import scala.collection.SortedMap
import de.saring.exerciseviewer.data._

case class Trajectory(val points : SortedMap[Trajectory.Timestamp,TrajectoryPoint]) {

  def startPoint = points.head._2

  def endPoint   = points.last._2

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
  , val latitude    : Option[Double] = None // in degrees
  , val longitude   : Option[Double] = None // in degrees
  , val heartRate   : Option[Short]  = None // in bpm
  , val altitude    : Option[Short]  = None // in meters?
  , val speed       : Option[Double] = None // in Km/h
  , val cadence     : Option[Short]  = None // in rpm
  , val power       : Option[Short]  = None // in watts ?
  , val temperature : Option[Short]  = None // in degrees celcius
  , val distance    : Option[Int]    = None // since start in meters
)


object TrajectoryPoint {
  def fromExerciseSample(es : ExerciseSample) =
    TrajectoryPoint ( es.getTimestamp()
                    , Option(es.getPosition().getLatitude())
                    , Option(es.getPosition().getLongitude())
                    , Option(es.getHeartRate())
                    , Option(es.getAltitude())
                    , Option(es.getSpeed())
                    , Option(es.getCadence())
                    , None // power
                    , Option(es.getTemperature())
                    , Option(es.getDistance()))

}
