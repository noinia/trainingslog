package net.fstaals.tl.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import Helpers._
import net.fstaals.tl.model._
import net.fstaals.tl.view._
import net.fstaals.tl._
import org.joda.time.Period

class ActivitySnippet(pk: PK)
      extends ModelSnippet[Activity](pk) with UserSnippet with StatefulSnippet {


  def canAccess(a: Activity) = true // AccessControl.activityIsViewable(a)

  def singleton = Activity

  val activity = getModel

  def dispatch = {
    case "summary" => summary
    case "graphs"  => graphs
    case "map"     => map
    case "details" => details
  }



  private def toForm[T <: Show](x: Option[T]) = {
    println(x)
    val s = (x map {_.show}).getOrElse("")
    println("s: " ++ s)
    <input type="text" readonly="readonly" value={s}/>
  }


  def summary = "#title"    #> activity.name._toForm    &
                "#isPublic" #> activity.isPublic._toForm &
                "#start"    #> activity.start._toForm      &
                "#duration" #> toForm(activity.duration map ShowablePeriod) &
                "#distance" #> toForm(activity.distance) &
                "#description" #> activity.description._toForm

  def graphs  = "#title"    #> "Graphs" &
                "#graphs"   #> "graphs"

  def map     = "#title"    #> "Map" &
                "#map"      #> "map"

  def details = "#title"    #> "Details" &
                "#details"  #> "details"

}
