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

  lazy val activityGraphs = new ActivityGraphs(activity)
  lazy val activityMap    = new ActivityMap(activity)

  def dispatch = {
    case "summary"    => summary
    case "graphs"     => graphs
    case "map"        => activityMap.render
    case "exercises"  => exercises
    case "controls"   => controls
    case "plotGraphs" => activityGraphs.render("graphArea")
    case "laps"       => laps
  }

  def save() = activity.validate match {
    case Nil if activity.isEditable => { activity.save ;S.notice("Saved.") }
    case Nil                        => {S.error("Forbidden.")}
    case xs                         => {S.error(xs)}
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
    val ts = new TagSelector(activity.tags.all) {
      override def add(t : Tag) = {
        println("Adding tag" + t)
        activity.tags += t
        println(activity.tags.all)
      }
    }

    "#tags *" #> ts.render
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

  def exercises = "#exerciseList *" #> (activity.exercises map {e =>
                                          (new ExerciseSnippet(e)).render }) &
                  (new AddExercise(activity.newExercise)).render

  def controls = "#controls *" #> "none yet"

  def laps = "#title" #> "laps"


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
