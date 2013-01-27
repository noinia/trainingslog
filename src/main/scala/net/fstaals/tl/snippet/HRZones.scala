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
