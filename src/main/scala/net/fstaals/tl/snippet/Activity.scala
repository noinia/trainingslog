package net.fstaals.tl.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import net.liftmodules.widgets.flot._
import net.liftweb.mapper.MixableMappedField


import Helpers._
import net.fstaals.tl.model._
import net.fstaals.tl._
import org.joda.time.Duration

import js._
import JsCmds._

class ActivitySnippet(val activity: Activity) extends StatefulSnippet {

  var inEditMode = false

  lazy val activityGraphs = new ActivityGraphs(activity)
  lazy val activityMap    = new ActivityMap(activity)

  def dispatch = {
    case "summary"    => summaryp
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

    // --------------------- Controls ------------------------------

  def controls = "#toggleEdit [onclick]" #> SHtml.ajaxInvoke(() => toggleEditMode)

  def toggleEditMode = {
    inEditMode = !inEditMode
    println(inEditMode)
    val res = summary.applyAgain
    println(res)

    SetHtml("summary",   res) &
    SetHtml("exercises", exercises.applyAgain)
  }

  // --------------------- Summary ------------------------------

  def summaryp(xhtml: NodeSeq) = {
    println(xhtml)

    summary(xhtml)
  }

  def summary = SHtml.memoize {
    general & timing & heartRate & power & elevation & cadence & temperature &
    saveButton
  }


  def saveButton = if (inEditMode) "#save"        #> SHtml.onSubmitUnit(save)
                   else            "#saveSummary" #> ""

  def show(x: MixableMappedField) =
    if (inEditMode) x.toForm else Full(x.asHtml)

  def general = "#title"         #> show(activity.name)                             &
                "#owner *"       #> (activity.owner.obj map {_.fullName} openOr "") &
                "#isPublic *"    #> show(activity.isPublic)                         &
                "#start"         #> activity.start                                  &
                "#duration *"    #> HhMmSs(activity.duration)                       &
                "#distance *"    #> Km(activity.distance)                           &
                "#description *" #> show(activity.description)                      &
                tags


  def tags = {
    val ts = new TagSelector(activity.tags.all) {
      override def add(t : Tag) = { activity.tags += t }
    }
    val render = if (inEditMode) ts.render else ts.renderReadOnly

    "#tags *" #> render
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

  // --------------------- Graphs ------------------------------

  def graphs  = "#title"    #> "Graphs"

  // --------------------- Exercises ---------------------------

  def exercises = SHtml.memoize(
    "#exerciseList *" #> (activity.exercises flatMap {e =>
                           Templates(List("templates-hidden","exercise")) map
                             (new ExerciseSnippet(e)).render}) &
    newExercise)

  def newExercise = if (inEditMode)
                      (new AddExercise(activity.newExercise)).render
                    else
                      "#addNewExercise ^^" #> ""

  // --------------------- Laps ------------------------------

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
