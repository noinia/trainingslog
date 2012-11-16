package net.fstaals.tl.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import Helpers._
import net.fstaals.tl.model._
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

  // def toForm[T <: Show](x: Option[T]) =
  //   <input type="text" readonly="readonly" value={Show.show(x)}/>

  // def toForm[T <: Show](x: T) =
  //   <input type="text" readonly="readonly" value={x.show}/>


  def summary =
    general & timing & heartRate & power & elevation & cadence & temperature

  def general = "#title"       #> activity.name._toForm                           &
                "#owner"       #> toForm(activity.owner.obj map {_.fullName})     &
                "#isPublic"    #> activity.isPublic._toForm                       &
                "#start"       #> activity.start                                  &
                "#duration *"  #> HhMmSs(activity.duration)                       &
                "#distance *"  #> Km(activity.distance)                           &
                "#description" #> activity.description._toForm                    &
                tags

  def tags = {
    val allTags : List[Tag] = Tag.myTags
    var unusedTags          = allTags
    var actTags : List[Tag] = activity.tags.all

    def load(s: String) = {

    }

    def add() = {

    }

    // <ul id="actTags">
    // <li id="actTag"></li>
    // </ul>
    // <datalist id="unusedTags">
    // <option id="unusedTag"></option>
    // </datalist>
    // <input list="unusedTags"
    // name="newTag">
    // <button id="addTag">Add</button>

    // "#tags"            #>                                              &
    "#actTag *"#> (actTags map {_.tag.get})                 &
    "option *" #> (unusedTags map {_.tag.get})
  }


  def timing = {
    val s = activity.speed
    "#movingTime *"     #> "?" &
    "#avgSpeed *"       #> Kmh(s map {_.avg}) &
    "#avgMovingSpeed *" #> "?" &
    "#maxSpeed *"       #> Kmh(s map {_.max})
  }

  def heartRate = orHide(activity.heartRate)("#heartRate") {hr=>
    "#avgHR *" #> Bpm(hr.avg)  &
    "#maxHR *" #> Bpm(hr.max)
  }

  def power = orHide(activity.power)("#power") {p =>
    "#avgPower *"  #> Watt(p.avg) &
    "#maxPower *"  #> Watt(p.max)
  }

  def elevation = orHide(activity.elevation)("elevation") {e=>
    "#elevationGain *"   #> Alt(e.gain) &
    "#elevationLoss *"   #> Alt(None)   &
    "#minElevation *"    #> Alt(e.min)  &
    "#maxElevation *"    #> Alt(e.max)
  }

  def cadence = orHide(activity.cadence)("#cadence") {c=>
    "#avgCad *" #> Rpm(c.avg) &
    "#maxCad *" #> Rpm(c.max)
  }

  def temperature = orHide(activity.temperature)("#temperature") {t=>
    "#avgTemp *" #> Celcius(t.avg) &
    "#minTemp *" #> Celcius(t.min) &
    "#maxTemp *" #> Celcius(t.max)
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
    def save() {
      println("SAVE BUTTON PRESSED")
    }

    "#addExercise" #> "foo" &
    "#save"        #> SHtml.onSubmitUnit(save)


// SHtml.a(Text("Add Exercise"),
                            //   showAddNewExercise)
  }




}

class AddExercise(val e: Exercise) {

  def render = {

    "#name"        #> e.name._toForm           &
    "#start"       #> HhMmSs(e.start.get)      &
    "#end"         #> HhMmSs(e.end.get)        &
    "#description" #> e.description._toForm    &
    "#save"        #> SHtml.onSubmitUnit(save)
  }

  def save() = {
    println("save button pressed.")
    println(e.name.get)
  }

}
