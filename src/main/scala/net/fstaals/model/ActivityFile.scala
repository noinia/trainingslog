package net.fstaals.model

import de.saring.exerciseviewer.parser.ExerciseParser
import de.saring.exerciseviewer.data.EVExercise
import de.saring.exerciseviewer.parser.ExerciseParserFactory

import org.joda.time.DateTime
import org.joda.time.Duration
import org.scala_tools.time.Imports._

case class ActivityFile(val path : String) {

  private var activityData : Option[EVExercise] = None

  var trajectory   : Trajectory = _

  // load the data, if something goes wrong simply ignore it.
  def load() {
    try {
      loadData()
    } catch {
      case e => {} // if something goes wrong ignore it.
    }
  }

  def hasData = activityData.isDefined

  // load the data. If something goes wrong, throw an exception.
  def loadData() = {
    val parser   = ExerciseParserFactory.getParser(path)
    val rawData  = parser.parseExercise(path)
    activityData = Some(rawData)
    trajectory   = Trajectory.fromEVSamples(rawData.getSampleList().toList)
  }

  def start : Option[DateTime]  = activityData map {ev => new DateTime(ev.getDate())}

  def end   : Option[DateTime]  = start map {
    // this trajectory should not be empty
    val dur = Duration.millis(trajectory.endPoint.timestamp)
    _ + dur
  }

//  def heartRate() = activityData map {_

  def avgSpeed : Option[Double] = activityData map {_.getSpeed().getSpeedAVG()}

  def maxSpeed : Option[Double] = activityData map {_.getSpeed().getSpeedMax()}

  def distance : Option[Int]    = activityData map {_.getSpeed().getDistance()}

  def altitude : Option[Int]    = activityData map {_.getAltitude()}

  def cadence : Option[Short]   = activityData map {_.getCadence()}

  def temperature() = activityData map {_.getTemperature()}

//  def laps() = activityData map {_.getLapList().toList()}

}
