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

  val graphs = graphsBy.flatten map {_.flotSerie}

  val globalOptions = new FlotOptions {
    override val legend = Full(new FlotLegendOptions {
      override val container = Full("graphLegend")
      override val noColumns = Full(graphs.length)
    })
  }

  def select[T](f: TrajectoryPoint => Option[T]) =
    a.trajectory map {tr => tr.select(f)} flatten

  def ifDef[A,B](x : Option[A])(y : B) : Option[B] =
    if (x.isDefined) Some(y) else None

  def graphsBy = // (xL: Showable[X]) =
    List( ifDef(a.heartRate)   (new HRGraph(select(_.heartRate),            simpleTS))
        , ifDef(a.speed)       (new SpeedGraph(select(_.speed),             simpleTS))
        , ifDef(a.elevation)   (new AltitudeGraph(select(_.altitude),       simpleTS))
        , ifDef(a.power)       (new PowerGraph(select(_.power),             simpleTS))
        , ifDef(a.cadence)     (new CadenceGraph(select(_.cadence),         simpleTS))
        , ifDef(a.temperature) (new TemperatureGraph(select(_.temperature), simpleTS))
        )

  def render(graphArea: String)(xhtml: NodeSeq) =
    Flot.render(graphArea, graphs,
                globalOptions, Flot.script(xhtml))

  // def graph(graphArea: String, graphs: List[FlotSerie])


}


class FlotGraph[X,Y]( val data      : Iterable[(X,Y)]
                    , val xLabeller : Showable[X]
                    , val yLabeller : Showable[Y]
                    ) {

  def flotData : List[(Double,Double)] =
    data map {case (x,y) => (toDouble(x),toDouble(y))} toList

  def flotSerie = new FlotSerie() {
    override val data = flotData
  }

  def toDouble(x: Any) = x match {
    case y : Double   => y
    case y : Number   => y.doubleValue
    case y : Duration => y.millis.doubleValue
  }

}

class HRGraph[X]( data : Iterable[(X,HeartRate)]
                , xLabeller : Showable[X]
                , yLabeller : Showable[HeartRate] = Bpm
                )
      extends FlotGraph[X,HeartRate](data, xLabeller, yLabeller) {

  override def flotSerie = new FlotSerie() {
    override val data  = flotData
    override val label = Full("Heart Rate")
    override val color = Full(Left("#d22132"))
  }
}

class SpeedGraph[X]( data : Iterable[(X,Speed)]
                   , xLabeller : Showable[X]
                   , yLabeller : Showable[Speed] = Kmh
                  )
      extends FlotGraph[X,Speed](data, xLabeller, yLabeller) {

  override def flotSerie = new FlotSerie() {
    override val data  = flotData
    override val label = Full("Speed")
    override val color = Full(Left("#3669da"))
  }
}

class AltitudeGraph[X]( data : Iterable[(X,Altitude)]
                      , xLabeller : Showable[X]
                      , yLabeller : Showable[Altitude] = Alt
                      )
      extends FlotGraph[X,Altitude](data, xLabeller, yLabeller) {

  override def flotSerie = new FlotSerie() {
    override val data  = flotData
    override val label = Full("Altitude")
    override val color = Full(Left("#228400"))
  }
}


class PowerGraph[X]( data : Iterable[(X,Power)]
                   , xLabeller : Showable[X]
                   , yLabeller : Showable[Power] = Watt
                   )
      extends FlotGraph[X,Power](data, xLabeller, yLabeller) {

  override def flotSerie = new FlotSerie() {
    override val data  = flotData
    override val label = Full("Power")
    override val color = Full(Left("#770077"))
  }
}


class CadenceGraph[X]( data : Iterable[(X,Cadence)]
                     , xLabeller : Showable[X]
                     , yLabeller : Showable[Cadence] = Rpm
                     )
      extends FlotGraph[X,Cadence](data, xLabeller, yLabeller) {

  override def flotSerie = new FlotSerie() {
    override val data  = flotData
    override val label = Full("Cadence")
    override val color = Full(Left("#ff4422"))
  }
}



class TemperatureGraph[X]( data : Iterable[(X,Temperature)]
                         , xLabeller : Showable[X]
                         , yLabeller : Showable[Temperature] = Celcius
                         )
      extends FlotGraph[X,Temperature](data, xLabeller, yLabeller) {

  override def flotSerie = new FlotSerie {
    override val data  = flotData
    override val label = Full("Temperature")
    override val color = Full(Left("#565656"))
  }
}
