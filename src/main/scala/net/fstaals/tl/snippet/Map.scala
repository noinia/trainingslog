package net.fstaals.tl.snippet

import scala.xml.NodeSeq

import org.joda.time.Duration

import com.github.nscala_time.time.Imports._

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import Helpers._

import js.JsCmds._
import js.JE.{JsRaw, JsArray}
import js.JsCmds.JsCrVar
import js.{JsObj, JE, JsCmd}
import JE._

import net.fstaals.tl.model._

import Trajectory._


class ActivityMap(val a: Activity) {

  def render = "#renderMap" #> googleMap

  def googleMap : NodeSeq = a.trajectory match {
    case Some(tr) => {
      // setup some locations to display on the map
      val locations = tr.toGoogleMapsTrajectory


      Script(Call("drawMap", ElemById("mapArea"),locations))
    }
    case None     => Nil
  }
}
