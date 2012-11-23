package net.fstaals.tl.snippet

import scala.collection.SortedMap

import scala.xml.NodeSeq

import org.joda.time.Duration
import org.scala_tools.time.Imports._

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import js.JsCmds._
import js.JE.{JsRaw, JsArray}
import js.JsCmds.JsCrVar
import js.{JsObj, JE, JsCmd}
import JE._

import net.fstaals.tl.model._
import net.fstaals.tl.helpers.Helpers._

import Trajectory._


class ActivityMap(val a: Activity) {

  def render = "#renderMap" #> googleMap

  def googleMap : NodeSeq = {

    // setup some locations to display on the map
    val locations: List[JsObj] = List(makeLocation("loc1","40.744715", "-74.0046"),makeLocation("loc2","40.75684", "-73.9966"))

    // where the magic happens
    Script(Call("drawMap", "#mapArea", ajaxFunc(locations)))

  }

  // converts a the location into a JSON Object
  def makeLocation(title: String, lat: String, lng: String): JsObj = {
    JsObj(("title", title),
      ("lat", lat),
      ("lng", lng))
  }

   // called by renderGoogleMap which passes the list of locations
   // into the javascript function as json objects
  def ajaxFunc(locobj: List[JsObj]) =
    JsObj(("loc", JsArray(locobj: _*)))



}
