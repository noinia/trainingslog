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

class ActivitySnippet(val activity: Activity) extends UserSnippet with StatefulSnippet {

  var editMode = false

  def dispatch = {
    case "summary" => summary
    case "graphs"  => graphs
    case "map"     => map
    case "details" => details
    case "main"    => main
  }

  def toForm(x: Box[String]) =
    <input type="text" readonly="readonly" value={x openOr ""}/>

  def toForm[T <: Show](x: Option[T]) =
    <input type="text" readonly="readonly" value={Show.show(x)}/>

  def toForm[T <: Show](x: T) =
    <input type="text" readonly="readonly" value={x.show}/>


  def summary =
    general & timing & heartRate & power & elevation & cadence & temperature

  def general = "#title"           #> activity.name._toForm                           &
                "#owner"           #> toForm(activity.owner.obj map {_.fullName})     &
                "#isPublic"        #> activity.isPublic._toForm                       &
                "#start"           #> activity.start                                  &
                "#duration *"      #> toForm(activity.duration map ShowablePeriod)    &
                "#distance"        #> toForm(activity.distance)                       &
                "#tags"            #> "?"                                             &
                "#description"     #> activity.description._toForm


  def timing = {
    val s = activity.speed
    "#movingTime"      #> "?" &
    "#avgSpeed"        #> toForm(s map {_.avg}) &
    "#avgMovingSpeed"  #> "?" &
    "#maxSpeed"        #> toForm(s map {_.max})
  }

  def heartRate = orHide(activity.heartRate)("#heartRate") {hr=>
    "#avgHR" #> toForm(hr.avg)  &
    "#maxHR" #> toForm(hr.max)
  }

  def power = orHide(activity.power)("#power") {p =>
    "#avgPower"  #> toForm(p.avg) &
    "#maxPower"  #> toForm(p.max)
  }

  def elevation = orHide(activity.elevation)("elevation") {e=>
    "#elevationGain"   #> toForm(e.gain) &
    "#elevationLoss"   #> toForm(None) &
    "#minElevation"    #> toForm(e.min) &
    "#maxElevation"    #> toForm(e.max)
  }

  def cadence = orHide(activity.cadence)("#cadence") {c=>
    "#avgCad" #> toForm(c.avg) &
    "#maxCad" #> toForm(c.max)
  }

  def temperature = orHide(activity.temperature)("#temperature") {t=>
    "#avgTemp" #> toForm(t.avg) &
    "#minTemp" #> toForm(t.min) &
    "#maxTemp" #> toForm(t.max)
  }

  def orHide[T](x: Option[T])(cssSel: String)(f: T => CssSel) = x match {
    case Some(y) => f(y)
    case _       => (cssSel ++ " [class+]") #> "hidden"
  }

  def newExercise = {
    def showNewEx() = {
      S.redirectTo("#addNewExercise")
    }
    "#newexercise" #> SHtml.onSubmitUnit(showNewEx)
  }



  def graphs  = "#title"    #> "Graphs"


  def map     = "#title"    #> "Map"

  def details = "#title"          #> "Details" &
                ".exercise *"     #> <h3>{activity.exercises map {_.name}}</h3>  &
                "#addNewExercise" #> (new AddExercise(activity.newExercise)).render

  def main = {
    "#addExercise" #> "foo" // SHtml.a(Text("Add Exercise"),
                            //   showAddNewExercise)
  }

}

class AddExercise(val e: Exercise) {

  def render = {

    "#name"        #> e.name._toForm           &
    "#start"       #> e.start._toForm          &
    "#end"         #> e.end._toForm            &
    "#description" #> e.description._toForm    &
    "#save"        #> SHtml.onSubmitUnit(save)
  }

  def save() = {
    println("save button pressed.")
    println(e.name.get)
  }

}
