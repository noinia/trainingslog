package net.fstaals.tl.model

import scala.collection.SortedMap
import de.saring.exerciseviewer.data._
import net.fstaals.tl.model.UnitTypes._

import Trajectory._

object Trajectory {

  type Timestamp = Long
  type VertexList = SortedMap[Trajectory.Timestamp,TrajectoryPoint]

  def fromEVSamples(xs : List[ExerciseSample]) =
    fromSeq(xs map TrajectoryPoint.fromExerciseSample)

  def fromSeq(xs: Seq[TrajectoryPoint]) = Trajectory(
    SortedMap.empty[Timestamp,TrajectoryPoint] ++ (xs map {p => (p.timestamp,p)}))

}

case class Trajectory(val points : VertexList) extends TrajectoryLike


trait TrajectoryLike {

  def points : VertexList

  def startPoint = startPointOption.get

  def endPoint   = endPointOption.get

  def startPointOption = points.headOption map {_._2}
  def endPointOption   = points.lastOption map {_._2}

  // this is almost groupBy, but not quite since we need to split each group
  // into contiguous pieces as well.
  def segment[B](f : TrajectoryPoint => B) : Map[B,SegmentedTrajectory] = {

    val empty = SegmentedTrajectory(Nil)

    def group(xs: List[TrajectoryPoint]) : Map[B,SegmentedTrajectory] = xs match {
      case Nil     => Map()
      case y :: ys => { val k         = f(y)
                        val (zs,rest) = ys span {x => k == f(x)}
                        val m         = group(rest)

                        m + (k -> (m.getOrElse(k,empty) :+ (y::zs)))
                      }
    }

    group(points.values.toList)
  }


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



// trait Segmentable

case class SegmentedTrajectory(val pieces : List[Trajectory]) extends TrajectoryLike {

  def :+ (xs : List[TrajectoryPoint]) =
    SegmentedTrajectory(Trajectory.fromSeq(xs) :: pieces)

  def ++ (xs: SegmentedTrajectory) =
    SegmentedTrajectory(pieces ++ xs.pieces)


  def points = pieces map {_.points} reduceRight {_++_}

  override def segment[B](f: TrajectoryPoint => B) = {

    def updateWith[K,V](xs : Map[K,V], k: K, v: V, f : (V,V) => V) : Map[K,V] =
      xs + (k -> ((xs.get(k) map {f(_,v)}).getOrElse(v)))

    def union(xs : Map[B,SegmentedTrajectory], ys: Map[B,SegmentedTrajectory]) =
      xs.foldRight(ys)({case ((k,v),a) =>
        updateWith(a,k,v,(b:SegmentedTrajectory, c:SegmentedTrajectory) => c ++ b)})

    pieces.foldRight(Map[B,SegmentedTrajectory]())({case (t,a)  => union(t.segment(f),a)})
  }

}
