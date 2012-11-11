package net.fstaals.tl.view

import org.joda.time.Period
import org.scala_tools.time.Imports._

trait Show {
  def show : String
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
