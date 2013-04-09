package net.fstaals.tl.snippet

import scala.collection.SortedMap

import scala.xml.NodeSeq

import org.joda.time.Duration
import org.scala_tools.time.Imports._

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import net.fstaals.tl.model._
import net.fstaals.tl.util._

import net.liftmodules.widgets.flot._

import Helpers._

import js._
import js.jquery._
import JE._
import JqJE._
import JsCmds._

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
                                   Full(90.0), Full(200.0) , Full("left")) //TODO fix max
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

  lazy val heartRateMarkings = (a.owner.obj.toList flatMap {_.hrZones}) map { z =>
    new FlotMarkings { override val ranges = List( FlotRange( "yaxis"
                                                            , z.lowerLimit.get - 1
                                                            , z.upperLimit.get
                                                           )
                                                 )
                       override val color  =
                         Full(ColorUtil.toBGColor(z.color.get, 0.65)) }
  }

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
        override val mode          = Full("x")
      })
      override val grid = Full(new FlotGridOptions {
        override val hoverable     = Full(true)
        override val autoHighlight = Full(false)
        override val markings      = heartRateMarkings
      })
  }


  def graphsBy[X](k : TrajectoryPoint => Option[X], xL : Showable[X])(implicit ord: Ordering[X]) = {
    def combineFst[A,B](t : (Option[A],B)) = t match {
      case (Some(_),b) => Some(b)
      case _           => None
    }
    activityGraphs(k,xL) map combineFst flatten
  }


  def selectedHandler(from: Double, to: Double) : JsCmd = a.trajectory match {
    case Some(tr) => { selection  = Some(tr.subtrajectory(from.round,to.round))
                      Jq(".selected") ~> JqReplace(selected.applyAgain)
                     }
    case _        => Noop
  }

  def unselectHandler() : JsCmd = {
    selection = None
    Jq(".selected") ~> JqReplace(selected.applyAgain)
  }

  def renderGraph = new CssSel {
    def apply(xhtml: NodeSeq) =
      Flot.render("graphArea", graphs map {_.flotSerie},
                  globalOptions(new FlotAxisOptions {
                    override val mode = Full(graphMode)
                  }, graphs map {_.axisOptions}),
                  Flot.script(xhtml),
                  FlotBindSelect((selectedHandler _).tupled),
                  FlotBindUnSelect(SHtml.ajaxInvoke(unselectHandler)),
                  FlotBindOnHover(Call("onHover",JsVar("plot_graphArea"))),
                  FlotLinkToMap
                )
  }


  var selection : Option[HasSummaryData] = None

  val selected = SHtml.memoize { "*" #> selectedCss  }

  def selectedCss = selection match {
    case Some(d) => (new SummaryData(d, d.duration,
                                     a.owner.obj.toList flatMap {_.hrZones},
                                     Nil // TODO: pwrZones
                                     )).render // TODO
    case _       => ("*" #> "")
  }

  def render = ".plotGraphs"       #> renderGraph       &
               "#graphIndicator *" #> indicatorTemplate

  def indicatorTemplate =
    Templates(List("templates-hidden","activity","indicator")) map indicator

  def indicator = ".selected *"      #> selected    &
                  ".crossHair"       #> renderUnits

  def renderUnits = ".unit *" #> ""
  //TODO: make a mapping that looks up the right units

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
      override val data      = flotData
      override val label     = l
      override val color     = c map {s => Left(s)}
      override val yaxis     = Full(yAxisIdx)
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


case class FlotBindUnSelect(cmd: JsCmd) extends FlotCapability {

  def render(flotInfo : FlotInfo) =
    JsRaw("""
      $(document).ready(function() {
        $("#%s").bind( "plotunselected", function( event ) {
          %s
        })
      })""".format(flotInfo.idPlaceholder, cmd.toJsCmd))

  def renderHide() = Noop
  def renderShow() = Noop
}

case class FlotBindSelect(selectedHandler: ((Double,Double)) => JsCmd)
     extends FlotCapability {

  def render(flotInfo : FlotInfo) =
    JsRaw("""
    $(document).ready(function() {
      $("#%s").bind( "plotselected", function( event, ranges ) {
        // from = ranges.xaxis.from, to = ranges.xaxis.to
        // similar for yaxis - with multiple axes, the extra ones are in
        // x2axis, x3axis, ...
        %s
      })
    })""".format(flotInfo.idPlaceholder, select.toJsCmd))


  def select = SHtml.ajaxCall(JsArray(JsVar("ranges.xaxis.from"),
                                      JsVar("ranges.xaxis.to")),
                              s => selectedHandler(prs(s)))

  def renderHide() = Noop
  def renderShow() = Noop

  def prs(s: String) = s.split(",").toList match {
    case x :: y :: Nil => (x.trim.toDouble, y.trim.toDouble)
  }

}

case class FlotBindOnHover(cmd: JsCmd) extends FlotCapability {

  def render(flotInfo : FlotInfo) = cmd

  def renderHide() = Noop
  def renderShow() = Noop
}


case object FlotLinkToMap extends FlotCapability {

  def render(flotInfo: FlotInfo) = {
    val graphsData = JsObj( ("graphArea", "#"+flotInfo.idPlaceholder)
                          , ("plot",JsVar("plot_"+flotInfo.idPlaceholder))
                          )
    // val mapData    = JsObj( ("currentPosition",JsVar("currentPostion"))
    //                       , ("map", JsVar("map"))
    //                       , ("locations", JsVar("locations"))
    //                       )
    Call("linkMapAndGraph",graphsData,JsVar("myMapData"))
  }

  def renderHide() = Noop
  def renderShow() = Noop
}
