package net.fstaals.tl.snippet

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import net.fstaals.tl.model._
import net.fstaals.tl.helpers.Helpers._
import Helpers._

class HRZones {

  var currentZones : List[HRZone] = HRZone.myHrZones
  var z : Box[HRZone]             = HRZone.newZone

  def render =
    "#name"    #> SHtml.onSubmit(s => z = z map {_.name(s)})               &
    "#color"   #> SHtml.onSubmit(s => z = z map {_.color(s)})              &
    "#lower"   #> SHtml.onSubmit(s => z = opt[Int](s.toInt) flatMap {i =>
                                             z map {_.lowerLimit(i)} })       &
    "#upper"   #> SHtml.onSubmit(s => z = opt[Int](s.toInt) flatMap {i =>
                                             z map {_.upperLimit(i)}})        &
    "#addZone" #> SHtml.onSubmitUnit(add)                                  &
    "#zones *" #> (currentZones map {zz =>
              ".name *"       #> zz.name.get       &
              ".color *"      #> zz.color.get      &
              ".lower *" #> zz.lowerLimit.get &
              ".upper *" #> zz.upperLimit.get })

  def add() = z map {_.validate} match {
    case Full(Nil) => { z map {_.save}
                       currentZones = (z ++ currentZones).toList
                       z = HRZone.newZone
                       S.notice("Zone added")
                     }
    case Full(xs)  => S.error(xs)
    case _         => S.error("Not logged in")
  }



}

class EditHRZone(z: HRZone) {

  private def toInt(s: String, default: Int) = opt[Int](s.toInt) getOrElse default

  def render =
    "#name"  #> SHtml.text(z.name, z.name        := _)               &
    "#color" #> SHtml.text(z.color, z.color      := _)              &
    "#lower" #> SHtml.number(z.lowerLimit, (i:Int) => z.lowerLimit := i, 0, 1000) &
    "#upper" #> SHtml.number(z.upperLimit, (i:Int) => z.upperLimit := i, 0, 1000) &
    "#save"  #> SHtml.onSubmitUnit(save)


  def save() = z.validate match {
    case Nil => { z.save ; S.notice("Saved") }
    case xs  => S.error(xs)
  }


}
