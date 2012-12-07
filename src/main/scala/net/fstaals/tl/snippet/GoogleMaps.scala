package net.fstaals.tl.snippet

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import js._
import js.JsCmds._
import JE._


trait GoogleMap extends JsExp {

  def mapArea    : String            = "mapArea"
  def mapOptions : GoogleMapOptions

  def toJsCmd = "new google.maps.Map(%s,%s)".format(mapArea,
                                                    mapOptions)



}

trait GoogleMapLatLong extends JsExp {

  def position : (Double,Double)

  def latitude  : Double = position._1
  def longitude : Double = position._2

  def toJsCmd = "new google.maps.LatLng(%f,%f)".format(latitude,longitude)

}

case class LatLong(override val latitude : Double, override val longitude : Double)
     extends GoogleMapLatLong  {
  def position = (latitude,longitude)
}

trait GoogleMapOptions extends JsExp {

  def center : GoogleMapLatLong

  def mapType : MapType = RoadMap
  def zoomLevel : Int   = 7


  def toJssCmd = JsObj( ("center", center)
                      , ("zoom",   zoomLevel)
                      , ("mapTypeId", mapType)
                      )
}


abstract class MapType extends JsExp {
  def toJsCmd = "google.maps.MapTypeId.%s".format(
    this.getClass.getSimpleName map {_.toUpper})
}


case object Hybrid   extends MapType
case object RoadMap  extends MapType
case object Satilite extends MapType
case object Terrain  extends MapType
