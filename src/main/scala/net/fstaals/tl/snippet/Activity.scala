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
  lazy val summaryData    = new SummaryData(activity,activity.duration)

  def dispatch = {
    case "summary"    => summary
    case "graphs"     => graphs
    case "map"        => activityMap.render
    case "exercises"  => exercises
    case "controls"   => controls
    case "plotGraphs" => activityGraphs.render("graphArea")
    case "laps"       => laps
  }

  def save() : JsCmd = activity.validate match {
    case Nil if activity.isEditable => { activity.save
                                         S.notice("Saved.")
                                         toggleEditMode }
    case Nil                        => {S.error("Forbidden.") }
    case xs                         => {S.error(xs) }
  }

    // --------------------- Controls ------------------------------

  def controls = toggleEdit

  def toggleEdit : CssSel =
    "#toggleEdit [onclick]" #> SHtml.ajaxInvoke(() => toggleEditMode)

  val enableEditButton = Button.button("Edit","toggleEdit")
  val cancelEditButton = CancelButton.button("Cancel","toggleEdit")

  def editModeText = (if (inEditMode)
                       "*" #> cancelEditButton.render
                     else
                       "*" #> enableEditButton.render) andThen toggleEdit

  def toggleEditMode = {
    inEditMode = !inEditMode

    val dummy = SHtml.hidden(() => Noop)

    Replace("toggleEdit", editModeText.apply(dummy)) & // the button replaces everything anyway
    Replace("summary",   summary.applyAgain)       &
    Replace("exercises", exercises.applyAgain)
  }

  // --------------------- Summary ------------------------------

  val summary = SHtml.memoize { general & summaryData.render & saveButton }

  def saveButton = if (inEditMode) "#save"        #> SHtml.onSubmitUnit(save)
                   else            "#saveSummary" #> ""

  def show(x: MixableMappedField) =
    if (inEditMode) x.toForm else Full(x.asHtml)

  def general = ".title"         #> show(activity.name)                             &
                ".owner *"       #> (activity.owner.obj map {_.fullName} openOr "") &
                ".isPublic *"    #> show(activity.isPublic)                         &
                ".start"         #> activity.start                                  &
                ".duration *"    #> HhMmSs(activity.duration)                       &
                ".distance *"    #> Km(activity.distance)                           &
                ".description *" #> show(activity.description)                      &
                tags


  def tags = {
    val ts = new TagSelector(activity.tags.all) {
      override def add(t: Tag) = { activity.tags += t }
      override def del(t: Tag) = { activity.tags -= t }
    }
    val render = if (inEditMode) ts.render else ts.renderReadOnly

    ".tags *" #> render
  }


  // --------------------- Graphs ------------------------------

  def graphs  = "#title"    #> "Graphs"

  // --------------------- Exercises ---------------------------

  val exercises = SHtml.memoize {
    "#exerciseList *" #> (activity.exercises flatMap {e =>
                           Templates(List("templates-hidden","exercise")) map
                             (new ExerciseSnippet(e)).render}) &
    newExercise
  }

  def newExercise = if (inEditMode)
                      (new AddExercise(activity.newExercise)).render
                    else
                      "#addNewExercise" #> ""

  // --------------------- Laps ------------------------------

  def laps = "#title" #> "laps"


}

class SummaryData(val data     : HasSummaryData,
                   total       : Option[Duration],
                   val hrZones : List[HRZone]  = Nil,
                   val pwrZones: List[PwrZone] = Nil) {

  val totalDuration = total map {_.getMillis()}

  def render = timing & heartRate & power & elevation & cadence & temperature

  def timing =
    ".movingTime *"     #> "?" &
    ".avgSpeed *"       #> Kmh(data.speed map {_.avg}) &
    ".avgMovingSpeed *" #> "?" &
    ".maxSpeed *"       #> Kmh(data.speed map {_.max})

  // displays the time and percentage of the total time of this segment.
  def segmentationFormat(x: Option[Duration]) = (x,totalDuration) match {
    case (Some(d),Some(t)) =>
      "%s (%.1f%%)".format(HhMmSs(d), (100.0 * d.getMillis()) / t)
    case (Some(d),_)       => HhMmSs(d)
    case _                 => ""
  }

  def heartRate = orHide(data.heartRate)(".heartRate") {hr=>
    ".hrzones *" #> // (data.trajectorySegmentsByHRZone map {case (z,tr) =>
    //   "label *"   #> z.name.get &
    //   ".field *"  #> segmentationFormat(tr.duration)
    // }) &
                                  "TODO"                      &
    ".avgHR *"   #> Bpm(hr.avg)  &
    ".maxHR *"   #> Bpm(hr.max)
  }

  def power = orHide(data.power)(".power") {p =>
    ".avgPower *"  #> Watt(p.avg) &
    ".maxPower *"  #> Watt(p.max)
  }

  def elevation = orHide(data.altitude)(".elevation") {e=>
    ".elevationGain *"   #> Alt(e.gain) &
    ".elevationLoss *"   #> Alt(None)   &
    ".minElevation *"    #> Alt(e.min)  &
    ".maxElevation *"    #> Alt(e.max)
  }

  def cadence = orHide(data.cadence)(".cadence") {c=>
    ".avgCad *" #> Rpm(c.avg) &
    ".maxCad *" #> Rpm(c.max)
  }

  def temperature = orHide(data.temperature)(".temperature") {t=>
    ".avgTemp *" #> Celcius(t.avg) &
    ".minTemp *" #> Celcius(t.min) &
    ".maxTemp *" #> Celcius(t.max)
  }

  def orHide[T](x: Option[T])(cssSel: String)(f: T => CssSel) = x match {
    case Some(y) => f(y)
    case _       => (cssSel ++ " [class+]") #> "hidden"
  }

}


class AddExercise(val e: Exercise) {

  def render = "#name"          #> e.name._toForm                       &
               "#start [value]" #> HhMmSs(e.start.get)                  &
               "#start"         #> SHtml.onSubmit(load(e.start := _) _) &
               "#end [value]"   #> HhMmSs(e.end.get)                    &
               "#end"           #> SHtml.onSubmit(load(e.end := _) _)   &
               "#rpe"           #> e.rpe._toForm                        &
               "#exdescr"       #> e.description._toForm                &
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
