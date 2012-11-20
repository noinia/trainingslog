package net.fstaals.tl.model

import net.liftweb.common._
import org.joda.time.{Duration, Period}
import org.scala_tools.time.Imports._

import UnitTypes._
import Parseable._

trait Showable[T] {

  def unit : String
  def strValue(x: T) : String = x.toString

  def apply(x : T)        : String = "%s %s".format(strValue(x),unit)
  def apply(x :Option[T]) : String = x map {apply(_)} getOrElse ""
  def apply(x: Box[T])    : String = x map {apply(_)} openOr ""
}

trait Parseable[T] {
  def parse(s : String) : Option[T]
}

object Parseable {
  import net.fstaals.tl.helpers.Helpers._

  def toInt(s: String)    = opt(s.trim.toInt)
  def toLong(s: String)   = opt(s.trim.toLong)
  def toDouble(s: String) = opt(s.trim.toDouble)
  def toShort(s: String)  = opt(s.trim.toShort)

}


object UnitTypes {
  type HeartRate   = Short  // unit: bpm
  type Altitude    = Int    // unit: meters
  type Speed       = Double // unit: kilometers / h
  type Distance    = Int    // unit: meters
  type Cadence     = Short  // unit: rpm
  type Power       = Short  // unit: watts
  type Temperature = Short  // unit: celcius
}


object Km extends Showable[Distance] with Parseable[Distance] {
  def unit                           = "Km"
  override def strValue(x: Distance) = "%.2f".format((x+0.0)/1000)
  def parse(s: String) = Parseable.toInt(s dropRight 2)
}

object Bpm extends Showable[HeartRate] with Parseable[HeartRate] {
  def unit = "bpm"
  def parse(s: String) = Parseable.toShort(s dropRight 3)
}


// I did not know a better name ...
object Alt extends Showable[Altitude] with Parseable[Altitude] {
  def unit = "m"
  def parse(s: String) = Parseable.toInt(s dropRight 1)
}

object Kmh extends Showable[Speed] with Parseable[Speed] {
  def unit                        = "km/h"
  override def strValue(x: Speed) = "%.2f".format(x)
  def parse(s: String) = Parseable.toDouble(s dropRight 4)
}

object Rpm extends Showable[Cadence] with Parseable[Cadence] {
  def unit = "rpm"
  def parse(s: String) = Parseable.toShort(s dropRight 3)
}

object Watt extends Showable[Power] with Parseable[Cadence] {
  def unit = "W"
  def parse(s: String) = Parseable.toShort(s dropRight 1)
}

object Celcius extends Showable[Temperature] with Parseable[Temperature] {
  def unit = "°C"
  def parse(s: String) = Parseable.toShort(s dropRight 2)
}

object HhMmSs extends Showable[Duration] with Parseable[Duration] {
  def unit = ""
  override def strValue(d: Duration) = {
    val p = new Period(d.millis)

    val h : Int = p.hours
    val m : Int = (p - h.hours).minutes
    val s : Int = (p - h.hours - m.minutes).seconds

    "%2d:%2d:%2d".format(h,m,s)
  }

  def parse(s: String) = s.split(":").toList flatMap Parseable.toInt match {
    case h :: m :: s :: Nil => Some(h.hours + m.minutes + s.seconds)
    case _                  => None
  }
}
