package net.fstaals.tl.snippet

import scala.collection.SortedMap

import scala.xml.NodeSeq

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import net.fstaals.tl.model._
import net.fstaals.tl.helpers.Helpers._

import net.liftmodules.widgets.flot._

import Trajectory._

class ActivityGraphs(val a: Activity) {

  val globalOptions = new FlotOptions {}

  def toFlotSerie[A,B](xs : Iterable[(A,B)]) = new FlotSerie() {
    override val data = xs map {case (a,b) => (toLong(a),toLong(b))} toList
  }

  def toLong(x: Any) = x match {
    case y : Long   => y.doubleValue
    case y : Double => y
    case y : Short  => y.doubleValue
  }

  lazy val speedByTime =
    toFlotSerie(a.trajectory map {tr => tr.select(_.speed)} flatten)

  lazy val hrByTime =
    toFlotSerie(a.trajectory map {tr => tr.select(_.heartRate)} flatten)

  lazy val tempByTime =
    toFlotSerie(a.trajectory map {tr => tr.select(_.temperature)} flatten)

  def speed(graphArea: String )(xhtml: NodeSeq) =
    Flot.render(graphArea, List(speedByTime, hrByTime, tempByTime),
                globalOptions, Flot.script(xhtml))

  // def graph(graphArea: String, graphs: List[FlotSerie])


}
