package net.fstaals.tl.snippet

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import net.fstaals.tl.model._
import net.fstaals.tl.helpers.Helpers._


class HRZones extends StatefulSnippet {

  var currentZones : List[HRZone] = User.hrZones
  var z : Box[HRZone]             = HRZone.newZone

  def dispatch = {
    case _ => render
  }

  def render =
    "#name"       #> SHtml.onSubmit(s => z = z map {_.name(s)})               &
    "#color"      #> SHtml.onSubmit(s => z = z map {_.color(s)})              &
    "#lowerLimit" #> SHtml.onSubmit(s => z = opt[Int](s.toInt) flatMap {i =>
                                             z map {_.lowerLimit(i)} })       &
    "#upperLimit" #> SHtml.onSubmit(s => z = opt[Int](s.toInt) flatMap {i =>
                                             z map {_.upperLimit(i)}})        &
    "#addZone"    #> SHtml.onSubmitUnit(add)                                  &
    "#zones *"    #> (currentZones map {zz =>
              ".name *"       #> zz.name.get       &
              ".color *"      #> zz.color.get      &
              ".lowerLimit *" #> zz.lowerLimit.get &
              ".upperLimit *" #> zz.upperLimit.get })

  def add() = z.toList flatMap {_.validate} match {
    case Nil => { z map {_.save} ; currentZones = (z ++ currentZones).toList }
    case xs  => S.error(xs)
  }



}
