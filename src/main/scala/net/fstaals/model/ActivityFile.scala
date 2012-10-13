package net.fstaals.model

import de.saring.exerciseviewer.parser.ExerciseParser
import de.saring.exerciseviewer.data.EVExercise
import de.saring.exerciseviewer.parser.ExerciseParserFactory

import org.scala_tools.time.Imports._

case class ActivityFile(val path : String) {

  var activityData : Option[EVExercise] = None

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
    val parser = ExerciseParserFactory.getParser(path)
    activityData = Some(parser.parseExercise(path))
  }

  def start() = activityData map {_.getDate()}

//  def end() =

  def distance() = speed map {_.getDistance()}

  def speed() = activityData map {_.getSpeed}

//  def heartRate() = activityData map {_

  def altitude() = activityData map {_.getAltitude()}

  def cadence() = activityData map {_.getCadence()}

  def temperature() = activityData map {_.getTemperature()}

//  def laps() = activityData map {_.getLapList().toList()}


}
