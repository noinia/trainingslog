package net.fstaals.tl.snippet

import scala.collection.SortedMap

import scala.xml.NodeSeq

import org.joda.time.Duration
import org.scala_tools.time.Imports._

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import net.fstaals.tl.model._
import net.fstaals.tl.helpers.Helpers._

import net.liftmodules.widgets.flot._

import UnitTypes._
import Trajectory._

class ActivityGraphs(val a: Activity) {

    // build the graphs
  def activityGraphs[X](k : TrajectoryPoint => Option[X],
                        xL : Showable[X])(implicit ord: Ordering[X]) = {
    type T = TrajectoryPoint ; val tr = a.trajectory

    List(
        (a.heartRate,
         tr map {FlotGraph(_,k,(p:T) => p.heartRate,   xL, Bpm,     "Heart Rate",  "#d22132")})
      , (a.speed,
         tr map {FlotGraph(_,k,(p:T) => p.speed,       xL, Kmh,     "Speed",       "#3669da")})
      , (a.elevation,
         tr map {FlotGraph(_,k,(p:T) => p.altitude,    xL, Alt,     "Altitude",    "#228400")})
      , (a.power,
         tr map {FlotGraph(_,k,(p:T) => p.power,       xL, Watt,    "Power",       "#770077")})
      , (a.cadence,
         tr map {FlotGraph(_,k,(p:T) => p.cadence,     xL, Rpm,     "Cadence",     "#ff4422")})
      , (a.temperature,
         tr map {FlotGraph(_,k,(p:T) => p.temperature, xL, Celcius, "Temperature", "#565656")})
  )}

  object simpleTS extends Showable[Timestamp] {
    def unit = ""
  }

  val byTime : TrajectoryPoint => Option[Timestamp] = tp => Some(tp.timestamp)

  val graphMode = "time"

  val graphs = graphsBy(byTime, simpleTS) zip (1 until 1000)

  def globalOptions(xAxis : FlotAxisOptions, yAxes : Seq[FlotAxisOptions]) =
    new FlotOptions {
      override val legend = Full(new FlotLegendOptions {
        override val container = Full("graphLegend")
        override val noColumns = Full(graphs.length)
      })
      override val xaxis = Full(xAxis)
      override val yaxes = Full(yAxes)
  }


  def graphsBy[X](k : TrajectoryPoint => Option[X], xL : Showable[X])(implicit ord: Ordering[X]) = {
    def combineFst[A,B](t : (Option[A],Option[B])) = t match {
      case (Some(_),Some(b)) => Some(b)
      case _                 => None
    }
    activityGraphs(k,xL) map combineFst flatten
  }

  def render(graphArea: String)(xhtml: NodeSeq) =
    Flot.render(graphArea, graphs map {case (g,i) => g.flotSerie(i)},
                globalOptions(new FlotAxisOptions {
                  override val mode = Full(graphMode)
                }, graphs map {t => t._1.axisOptions(t._2)}), Flot.script(xhtml))

}


object FlotGraph {

  def apply[X,Y]( tr : Trajectory
                , k  : TrajectoryPoint => Option[X]
                , f  : TrajectoryPoint => Option[Y]
                , xLabeller : Showable[X]
                , yLabeller : Showable[Y]
                , label     : String
                , color     : String)(implicit ord: Ordering[X]) : FlotGraph[X,Y] =
    apply(tr, k, f, xLabeller, yLabeller, Full(label), Full(color))


  def apply[X,Y]( tr : Trajectory
                , k  : TrajectoryPoint => Option[X]
                , f  : TrajectoryPoint => Option[Y]
                , xLabeller : Showable[X]
                , yLabeller : Showable[Y]
                , label     : Box[String]
                , color     : Box[String])(implicit ord: Ordering[X]) : FlotGraph[X,Y] =
    new FlotGraph(tr.select(k)(f), xLabeller, yLabeller, label, color)

}

class FlotGraph[X,Y]( val data      : Iterable[(X,Y)]
                    , val xLabeller : Showable[X]
                    , val yLabeller : Showable[Y]
                    , val label     : Box[String]
                    , val color     : Box[String]
                    ) {

  lazy val flotData : List[(Double,Double)] =
    data map {case (x,y) => (toDouble(x),toDouble(y))} toList

  def flotSerie(yAxisIdx: Int) = {
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

  def axisOptions(yAxisId: Int) = new FlotAxisOptions {
    override val min       = Full(flotData map {_._2} min) // TODO: subtract a bit
    override val max       = Full(flotData map {_._2} max) // TODO: add a bit
    override val tickColor = color
    override val position  = Full(if (yAxisId % 2 == 1) "left" else "right")
  }

}

case class FlotXAxis[X,Y](generator: TrajectoryPoint => Option[X],
                          xL : Showable[X],
                          options : FlotAxisOptions)(implicit ord: Ordering[X]) {

}
