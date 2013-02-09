package net.fstaals.tl.snippet

import scala.collection.SortedMap

import scala.xml.NodeSeq

import org.joda.time.Duration
import org.scala_tools.time.Imports._

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import net.fstaals.tl.model._
// import net.fstaals.tl.helpers.Helpers._

import net.liftmodules.widgets.flot._

import Helpers._

import UnitTypes._
import Trajectory._

class ActivityGraphs(val a: Activity) {



    // build the graphs
  def activityGraphs[X](k : TrajectoryPoint => Option[X], xL : Showable[X])
                       (implicit ord: Ordering[X]) = a.trajectory match {
    case None     => Nil
    case Some(tr) => { type T = TrajectoryPoint ; type FAO = FlotAxisOptions ; List(
        a.heartRate   -> FlotGraph(tr,k,(p:T) => p.heartRate, xL, Bpm,
                                   "Heart Rate",  "#d22132", 1,
                                   Full(0.0), Full(200.0) , Full("left")) //TODO fix max
      , a.speed       -> FlotGraph(tr,k,(p:T) => p.speed, xL, Kmh,
                                   "Speed", "#3669da", 2,
                                   Full(0.0), Full(60.0), Full("left")) //TODO fix max
      , a.altitude    -> FlotGraph(tr,k,(p:T) => p.altitude, xL, Alt,
                                   "Altitude", "#228400", 3,
                                   Empty, Empty, Full("right"))
      , a.power       -> FlotGraph(tr,k,(p:T) => p.power, xL, Watt,
                                   "Power", "#770077", 4,
                                   Empty, Empty, Full("right"))
      , a.cadence     -> FlotGraph(tr,k,(p:T) => p.cadence, xL, Rpm,
                                   "Cadence","#ff4422", 5,
                                   Empty, Empty, Full("left"))
      , a.temperature -> FlotGraph(tr,k,(p:T) => p.temperature, xL, Celcius,
                                   "Temperature", "#565656", 6,
                                   Empty, Empty, Full("right"))
  )}}


  object simpleTS extends Showable[Timestamp] {
    def unit = ""
  }

  val byTime : TrajectoryPoint => Option[Timestamp] = tp => Some(tp.timestamp)

  val graphMode = "time"

  val graphs = graphsBy(byTime, simpleTS)

  def globalOptions(xAxis : FlotAxisOptions, yAxes : Seq[FlotAxisOptions]) =
    new FlotOptions {
      override val legend = Full(new FlotLegendOptions {
        override val container = Full("graphLegend")
        override val noColumns = Full(graphs.length)
      })
      override val xaxis = Full(xAxis)
      override val yaxes = Full(yAxes)
      override val modeSelection = Full("x")
      override val crossHair = Full(new FlotCrossHairOptions {
        override val mode = Full("x")
      })
  }


  def graphsBy[X](k : TrajectoryPoint => Option[X], xL : Showable[X])(implicit ord: Ordering[X]) = {
    def combineFst[A,B](t : (Option[A],B)) = t match {
      case (Some(_),b) => Some(b)
      case _           => None
    }
    activityGraphs(k,xL) map combineFst flatten
  }

  def render(graphArea: String)(xhtml: NodeSeq) =
    Flot.render(graphArea, graphs map {_.flotSerie},
                globalOptions(new FlotAxisOptions {
                  override val mode = Full(graphMode)
                }, graphs map {_.axisOptions}), Flot.script(xhtml))

  def selected = (new SummaryData(a,a.duration)).render // TODO

}






object FlotGraph {

  def apply[X,Y]( tr : TrajectoryLike
                , k  : TrajectoryPoint => Option[X]
                , f  : TrajectoryPoint => Option[Y]
                , xLabeller   : Showable[X]
                , yLabeller   : Showable[Y]
                , label       : String
                , color       : String
                , yAxisIdx    : Int
                , minY        : Box[Double]
                , maxY        : Box[Double]
                , axPosition  : Box[String]
                )(implicit ord: Ordering[X]) : FlotGraph[X,Y] =
    apply(tr, k, f, xLabeller, yLabeller, Full(label), Full(color),yAxisIdx,
          minY, maxY, axPosition)

  def apply[X,Y]( tr : TrajectoryLike
                , k  : TrajectoryPoint => Option[X]
                , f  : TrajectoryPoint => Option[Y]
                , xLabeller : Showable[X]
                , yLabeller : Showable[Y]
                , label     : Box[String]
                , color     : Box[String]
                , yAxisIdx    : Int
                , minY        : Box[Double]
                , maxY        : Box[Double]
                , axPosition  : Box[String])(implicit ord: Ordering[X]) : FlotGraph[X,Y] =
    new FlotGraph(tr.select(k)(f),
                  xLabeller, yLabeller, label, color, yAxisIdx,
                  minY, maxY, axPosition)

}

class FlotGraph[X,Y]( val data        : Iterable[(X,Y)]
                    , val xLabeller   : Showable[X]
                    , val yLabeller   : Showable[Y]
                    , val label       : Box[String]
                    , val color       : Box[String]
                    , val yAxisIdx    : Int
                    , val minY        : Box[Double]
                    , val maxY        : Box[Double]
                    , val axPosition  : Box[String]
                    ) {

  lazy val flotData : List[(Double,Double)] =
    data map {case (x,y) => (toDouble(x),toDouble(y))} toList

  def flotSerie = {
    val l = label
    val c = color
    new FlotSerie() {
      override val data = flotData
      override val label = l
      override val color = c map {s => Left(s)}
      override val yaxis = Full(yAxisIdx)
    }
  }

  def isEmpty = flotData.isEmpty

  def toDouble(x: Any) = x match {
    case y : Double   => y
    case y : Number   => y.doubleValue
    case y : Duration => y.millis.doubleValue
  }

  def axisOptions = new FlotAxisOptions {
    override val min       = minY
    override val max       = maxY
    override val position  = axPosition
    override val tickColor = color
  }

}




case class FlotXAxis[X,Y](generator: TrajectoryPoint => Option[X],
                          xL : Showable[X],
                          options : FlotAxisOptions)(implicit ord: Ordering[X]) {

}
