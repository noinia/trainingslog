package net.fstaals.tl.model

import scala.collection.SortedMap
import de.saring.exerciseviewer.data._

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
                   , Option(es.getHeartRate())   map HeartRate
                   , Option(es.getAltitude())    map {s => Altitude(s.intValue)}
                   , Option(es.getSpeed())       map {f => Speed(f.doubleValue)}
                   , Option(es.getCadence())     map Cadence
                   , None // power
                   , Option(es.getTemperature()) map Temperature
                   , Option(es.getDistance())    map Distance
                   )


}

trait HasUnit[T] {
  def defaultUnit : String
  def unit = defaultUnit
  def value : T

  def toStrWithUnit : String = value.toString ++ unit
}

object HasUnit {
  implicit def shortToHr             = HeartRate
  implicit def shortToAlt(s:Short)   = Altitude(s.intValue)
  implicit def intToAlt              = Altitude
  implicit def doubleToSpeed         = Speed
  implicit def floatToSpeed(f:Float) = Speed (f.doubleValue)
  implicit def intToDistance         = Distance
  implicit def shortToCadence        = Cadence
  implicit def shortToTemp           = Temperature
}


case class HeartRate(val hr: Short) extends HasUnit[Short] {
  def defaultUnit = "B/m"
  def value = hr
}

case class Altitude(val a: Int) extends HasUnit[Int] {
  def defaultUnit = "m"
  def value = a
}

case class Speed(val s: Double) extends HasUnit[Double] {
  def defaultUnit = "Km/h"
  def value = s
}

case class Distance(val m: Int) extends HasUnit[Int] {
  def defaultUnit = "m"
  def value = m
}

case class Cadence(val c: Short) extends HasUnit[Short] {
  def defaultUnit = "r/m"
  def value = c
}

case class Power(val p: Short) extends HasUnit[Short] {
  def defaultUnit = "W"
  def value = p
}

case class Temperature(val t: Short) extends HasUnit[Short] {
  def defaultUnit = "C" //TODO, the circle thingy
  def value = t
}
