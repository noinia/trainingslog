package net.fstaals.tl.view

import net.liftweb.common._
import org.joda.time.Period
import org.scala_tools.time.Imports._

trait Show {
  def show : String
}

object Show {
  def show[T <: Show](x: Option[T]) = x map {_.show} getOrElse ""
  def show[T <: Show](x: Box[T])    = x map {_.show} openOr ""
}


trait HasUnit extends Show {
  def defaultUnit : String
  def unit = defaultUnit
  def strValue : String

  def show = strValue ++ " " ++ unit
}

case class ShowablePeriod(val d: Period) extends Show {
  def show = {
    // this stuff with periods is anoyging

    val h : Int = d.toStandardHours.getHours()
    val m : Int = (d - h.hours).toStandardMinutes().getMinutes()
    val s : Int = (d - h.hours - m.minutes).toStandardSeconds().getSeconds()

    "%2d:%2d:%2d".format(h,m,s)
  }

}
