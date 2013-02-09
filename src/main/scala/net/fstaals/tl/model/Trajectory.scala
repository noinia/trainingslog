package net.fstaals.tl.model

import scala.collection.SortedMap
import de.saring.exerciseviewer.data._
import net.fstaals.tl.model.UnitTypes._

import net.liftweb.common._


import Trajectory._

import net.liftweb.http.js._
import JE._

import org.joda.time.Duration


trait HasSummaryData {
  def duration    : Option[Duration]
  def speed       : Option[SpeedSummary]
  def heartRate   : Option[HeartRateSummary]
  def distance    : Option[Distance]
  def altitude    : Option[AltitudeSummary]
  def cadence     : Option[CadenceSummary]
  def temperature : Option[TemperatureSummary]
  def power       : Option[PowerSummary]

  // get a trajectory for this thing
  def trajectory  : Option[TrajectoryLike]
}



object Trajectory {

  type Timestamp  = Long
  type SM[T]      = SortedMap[Timestamp,T]
  type VertexList = SortedMap[Timestamp,TrajectoryPoint]

  def fromEVSamples(xs : List[ExerciseSample]) =
    fromSeq(xs map TrajectoryPoint.fromExerciseSample)

  def fromSeq(xs: Seq[TrajectoryPoint]) = Trajectory(
    SortedMap.empty[Timestamp,TrajectoryPoint] ++ (xs map {p => (p.timestamp,p)}))

}

case class Trajectory(val points : VertexList) extends TrajectoryLike

trait TrajectoryLike extends HasSummaryData {

  def points : VertexList

  def trajectory = Some(this)

  def startPoint = startPointOption.get

  def endPoint   = endPointOption.get

  def duration = (points.headOption, points.lastOption) match {
    case (Some((s,_)),Some((e,_))) => Some(new Duration(e - s))
    case _                         => None
  }

  def startPointOption = points.headOption map {_._2}
  def endPointOption   = points.lastOption map {_._2}

  private def gather[X](f: TrajectoryPoint => Option[X]) : Option[List[X]] =
    points.values.toList.flatMap(y => f(y).toList) match {
      case Nil => None
      case xs  => Some(xs)
  }

  private def avg(xs: Seq[Short])  : Short  = avg(xs).round.shortValue
  private def avg(xs: Seq[Int])    : Int    = avg(xs).round
  private def avg(xs: Seq[Double]) : Double = xs.sum / xs.length

  def speed : Option[SpeedSummary]            =
    gather(t => t.speed) map {xs => SpeedSummary(avg(xs),xs.sum)}

  def distance : Option[Distance]              =
    None // TODO

  def heartRate : Option[HeartRateSummary] =
    gather(t => t.heartRate) map { xs => HeartRateSummary(avg(xs),xs.max)}

  def altitude : Option[AltitudeSummary]       =
    gather(t => t.altitude) map {xs =>
      AltitudeSummary(avg(xs),xs.min,xs.max,0)} //TODO: GAIN ?

  def cadence : Option[CadenceSummary]         =
    gather(t => t.cadence) map {xs => CadenceSummary(avg(xs), xs.max)}

  def temperature : Option[TemperatureSummary] =
    gather(t => t.temperature) map {xs =>
      TemperatureSummary(avg(xs), xs.min,xs.max) }

  def power : Option[PowerSummary] = None


  /**
   * This is similar to groupBy. However there are two important differences:
   *
   *  (1) we (need to) make sure that each group consists of contiguous pieces itself.

   * (2) The last trajectoryPoint in each segment may *NOT* satisfy the
   *  classification[1]. We do this to make sure we do not lose trajectory length.
   *  I.e. if we were to split the trajectory in between vertices v_i and v_{i+1},
   *  then we do not want to lose the the edge (v_i,v_{i+1}) itself.
   *
   * stated differently. The result of tr.segment(f) is a Map with key value pairs
   * (k,vs), where vs is a list of trajectories with time interval of the form [s,e)
   *
   *
   * [1] and in general will not satisfy this classification, the only segment that
   * in which the last vertex of a segment will satisfy the classification is the last
   * segment
   */
  def segment[B](f : TrajectoryPoint => B) : Map[B,SegmentedTrajectory] = {

    val empty = SegmentedTrajectory(Nil)

    def group(xs: List[TrajectoryPoint]) : Map[B,SegmentedTrajectory] = xs match {
      case Nil     => Map()
      case y :: ys => { val k         = f(y)
                        val (zs,rest) = ys span {x => k == f(x)}
                        val zsp       = rest.headOption.toList
                        val m         = group(rest)

                        m + (k -> (m.getOrElse(k,empty) :+ (y::zs++zsp)))
                      }
    }

    group(points.values.toList)
  }

  def select[K,V](k: TrajectoryPoint => Option[K])(f : TrajectoryPoint => Option[V])(implicit ord: Ordering[K]) : SortedMap[K,V] =
    SortedMap.empty[K,V] ++ (points flatMap {case (_,p) => (k(p),f(p)) match {
      case (Some(x),Some(y)) => Some((x,y))
      case _                 => None
    }})


  def toGoogleMapsTrajectory =
    JsArray(points.values.toList flatMap {_.toGoogleMapsLatLng})


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
) {


  // get a googleMaps LatLn js object representing this trajectory point
  def toGoogleMapsLatLng : Option[JsExp] =
    (latitude,longitude) match {
      case (Some(lat),Some(long)) =>
        Some(JsRaw("new google.maps.LatLng(%f,%f)".format(lat,long)))
      case _                      => None
    }


}

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



// trait Segmentable

case class SegmentedTrajectory(val pieces : List[Trajectory]) extends TrajectoryLike {

  def :+ (xs : List[TrajectoryPoint]) =
    SegmentedTrajectory(Trajectory.fromSeq(xs) :: pieces)

  def ++ (xs: SegmentedTrajectory) =
    SegmentedTrajectory(pieces ++ xs.pieces)


  def points = pieces map {_.points} reduceRight {_++_}

  override def duration =
    Some((pieces flatMap {_.duration}).foldRight(Duration.ZERO)({case (d1,d2) =>
                                                                  d1.plus(d2)}))

  override def segment[B](f: TrajectoryPoint => B) = {

    def updateWith[K,V](xs : Map[K,V], k: K, v: V, f : (V,V) => V) : Map[K,V] =
      xs + (k -> ((xs.get(k) map {f(_,v)}).getOrElse(v)))

    def union(xs : Map[B,SegmentedTrajectory], ys: Map[B,SegmentedTrajectory]) =
      xs.foldRight(ys)({case ((k,v),a) =>
        updateWith(a,k,v,(b:SegmentedTrajectory, c:SegmentedTrajectory) => c ++ b)})

    pieces.foldRight(Map[B,SegmentedTrajectory]())({case (t,a)  => union(t.segment(f),a)})
  }




}



object TrajectorySegmenters {

  def byHrZone(tr: TrajectoryLike, zones : List[HRZone]) = {

    def zone(tp: TrajectoryPoint) : HRZone = {
      val unknownZ = HRZone.create.name("Other").lowerLimit(1000)
      tp.heartRate match {
        case Some(h) => zones filter {_.inZone(h)} match {
          case z :: _  => z
          case _       => unknownZ
        }
        case _       => unknownZ
      }}

    tr.segment(zone _).toList sortBy {_._1.lowerLimit.get}
  }



}
