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

  object simpleTS extends Showable[Timestamp] {
    def unit = ""
  }

  val byTime : TrajectoryPoint => Option[Timestamp] = tp => Some(tp.timestamp)

  val graphs =
    graphsBy(byTime, simpleTS).flatten map {_.flotSerie}

  val globalOptions = new FlotOptions {
    override val legend = Full(new FlotLegendOptions {
      override val container = Full("graphLegend")
      override val noColumns = Full(graphs.length)
    })
  }

  def graphsBy[X](k : TrajectoryPoint => Option[X], xL : Showable[X])(implicit ord: Ordering[X]) = {
    val tr = a.trajectory
    type T = TrajectoryPoint

    // build the graphs
    val graphs = List(
      tr map {FlotGraph(_,k,(p:T) => p.heartRate,   xL, Bpm,     "Heart Rate",  "#d22132")}
    , tr map {FlotGraph(_,k,(p:T) => p.speed,       xL, Kmh,     "Speed",       "#3669da")}
    , tr map {FlotGraph(_,k,(p:T) => p.altitude,    xL, Alt,     "Altitude",    "#228400")}
    , tr map {FlotGraph(_,k,(p:T) => p.power,       xL, Watt,    "Power",       "#770077")}
    , tr map {FlotGraph(_,k,(p:T) => p.cadence,     xL, Rpm,     "Cadence",     "#ff4422")}
    , tr map {FlotGraph(_,k,(p:T) => p.temperature, xL, Celcius, "Temperature", "#565656")}
    )

    def combineFst[A,B](t : (Option[A],Option[B])) = t match {
      case (Some(a),Some(_)) => Some(a)
      case _                 => None
    }


    // the globals deterime if we should show the corresponding graph: if
    // they are defined then we show the graph with the data, otherwise we don't
    val globals = List(a.heartRate,a.speed,a.elevation,a.power,a.cadence,a.temperature)

    graphs zip globals map combineFst
  }

  def render(graphArea: String)(xhtml: NodeSeq) =
    Flot.render(graphArea, graphs,
                globalOptions, Flot.script(xhtml))

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

  println(label ++ ": " ++ flotData.take(10))

  def flotSerie = {
    val l = label
    val c = color
    new FlotSerie() {
      override val data = flotData
      override val label = l
      override val color = c map {s => Left(s)}
    }
  }

  def isEmpty = flotData.isEmpty

  def toDouble(x: Any) = x match {
    case y : Double   => y
    case y : Number   => y.doubleValue
    case y : Duration => y.millis.doubleValue
  }

}
