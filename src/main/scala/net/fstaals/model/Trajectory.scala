package net.fstaals.model

import de.saring.exerciseviewer.data._

case class Trajectory(val points : SortedMap[Trajectory.Timestamp,TrajectoryPoint]) {

}

object Trajectory {
  type Timestamp = Long

  def fromEVSamples(xs : List[ExerciseSample]) =
    Trajectory(map {val p = TrajectoryPoint.fromExerciseSample(_)
                    (p.timestamp,p)})

}


class TrajectoryPoint(
    val timestamp   : Long                  // time since start in 1/1000 sec
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
  def fromExerciseSample(es : ExerciseSample) = apply( es.getTimestamp()
                                                     , es.getPosition().getLatitude()
                                                     , es.getPosition().getLongitude()
                                                     , es.getHeartRate()
                                                     , es.getAltitude()
                                                     , es.getSpeed()
                                                     , es.getCadence()
                                                     , es.getDistance()
                                                     , es.getTemperature())

  def apply( timestamp   : Long
           , latitude    : Double = null
           , longitude   : Double = null
           , heartRate   : Short  = null
           , altitude    : Short  = null
           , speed       : Double = null
           , cadence     : Short  = null
           , power       : Short  = null
           , temperature : Short  = null
           , distance    : Int    = null) {
    def w[T](x: T) = if (x != null) Some[T](x) else Nothing

    new TrajectoryPoint( timestamp
                       , w(latitude)
                       , w(longitude)
                       , w(heartRate)
                       , w(altitude)
                       , w(speed)
                       , w(cadence)
                       , w(power)
                       , w(temperature)
                       , w(distance))
  }



}
