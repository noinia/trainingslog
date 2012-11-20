package net.fstaals.tl.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import net.liftmodules.widgets.flot._

import Helpers._
import net.fstaals.tl.model._
import net.fstaals.tl._
import org.joda.time.Duration

class ActivitySnippet(val activity: Activity) extends UserSnippet with StatefulSnippet {

  var inEditMode = false

  def dispatch = {
    case "summary"  => summary
    case "graphs"   => graphs
    case "map"      => map
    case "details"  => details
    case "controls" => controls
    case "sine"     => sine
  }

  def save() = {
    println("save activity")
    println(activity.name)
  }


  def summary =
    general & timing & heartRate & power & elevation & cadence & temperature &
    "#save" #> SHtml.onSubmitUnit(save)

  def general = "#title"       #> activity.name._toForm                           &
                "#owner *"     #> (activity.owner.obj map {_.fullName} openOr "") &
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

  def sine = (new ActivityGraphs(activity)).speed("graphArea") _

  // def sine(xhtml: NodeSeq) = {
  //   val data_values: List[(Double,Double)] = for (i <- List.range (0, 140, 5))
  //       yield (i / 10.0, Math.sin(i / 10.0) )

  //   val data_to_plot = new FlotSerie() {
  //       override val data = data_values
  //   }

  //   Flot.render ( "graph_area", List(data_to_plot), new FlotOptions {}, Flot.script(xhtml))
  // }


  def map     = "#title"    #> "Map"

  def details =
    "#title"          #> "Details" &
    "#numex"          #> activity.exercises.length &
    "#exercises *"    #> (activity.exercises map {e =>
                          (new ExerciseSnippet(e)).render }) &
    (new AddExercise(activity.newExercise)).render

  def controls = "#controls *" #> "none yet"


}

class AddExercise(val e: Exercise) {

  def render = "#name"          #> e.name._toForm                       &
               "#start [value]" #> HhMmSs(e.start.get)                  &
               "#start"         #> SHtml.onSubmit(load(e.start := _) _) &
               "#end [value]"   #> HhMmSs(e.end.get)                    &
               "#end"           #> SHtml.onSubmit(load(e.end := _) _)   &
               "#rpe"           #> e.rpe._toForm                        &
               "#description"   #> e.description._toForm                &
               "#save"          #> SHtml.onSubmitUnit(save)

  def load(f : Duration => Duration)(s: String) = HhMmSs.parse(s) match {
    case Some(d) => {f(d)}
    case _       => {}
  }

  def save() = e.validate match {
    case Nil if e.isEditable => { e.save ; S.notice("Saved.") }
    case Nil                 => {S.error("Forbidden.")}
    case xs                  => {S.error(xs)}
  }

}

class ExerciseSnippet(val e: Exercise) {

  def render = ".name *"          #> e.name.get                           &
               ".start *"         #> HhMmSs(e.start.get)                  &
               ".end *"           #> HhMmSs(e.end.get)                    &
               ".rpe *"           #> e.rpe.get                            &
               ".description *"   #> e.description.asHtml

}
