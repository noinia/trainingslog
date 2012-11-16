package net.fstaals.tl.model

import net.liftweb.common._
import org.joda.time.{Duration, Period}
import org.scala_tools.time.Imports._

import UnitTypes._

trait Showable[T] {

  def unit : String
  def strValue(x: T) : String = x.toString

  def apply(x : T)        : String = "%s %s".format(strValue(x),unit)
  def apply(x :Option[T]) : String = x map {apply(_)} getOrElse ""
  def apply(x: Box[T])    : String = x map {apply(_)} openOr ""
}


object UnitTypes {
  type HeartRate   = Long   // unit: bpm
  type Altitude    = Int    // unit: meters
  type Speed       = Double // unit: kilometers / h
  type Distance    = Int    // unit: meters
  type Cadence     = Short  // unit: rpm
  type Power       = Short  // unit: watts
  type Temperature = Short  // unit: celcius
}

object Km extends Showable[Distance] {
  def unit                           = "Km"
  override def strValue(x: Distance) = "%.2f".format((x+0.0)/1000)
}

object Bpm extends Showable[HeartRate] {
  def unit = "bpm"
}


// I did not know a better name ...
object Alt extends Showable[Altitude] {
  def unit = "m"
}

object Kmh extends Showable[Speed] {
  def unit                        = "Km/h"
  override def strValue(x: Speed) = "%.2f".format(x)
}

object Rpm extends Showable[Cadence] {
  def unit = "rpm"
}

object Watt extends Showable[Power] {
  def unit = "W"
}

object Celcius extends Showable[Temperature] {
  def unit = "°C"
}

object HhMmSs extends Showable[Duration] {
  def unit = ""
  override def strValue(d: Duration) = {
    val p = new Period(d.millis)

    val h : Int = p.hours
    val m : Int = (p - h.hours).minutes
    val s : Int = (p - h.hours - m.minutes).seconds

    "%2d:%2d:%2d".format(h,m,s)
  }
}
