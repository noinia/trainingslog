package net.fstaals.tl

import net.liftweb._
import sitemap._
import Loc._

import net.liftweb.mapper._
import net.liftweb.http._
import net.liftweb.util._
import Helpers._
import net.liftweb.common._

import net.fstaals.tl.model._

object TLSiteMap {

  def idParser(xs: List[String]) = try {
    Full(xs.head.toLong)
  } catch {case _ => Empty}

  def idEncoder[T <: IdPK](x : T) = List(x.id.get.toString)

  def objParser[T <: LongKeyedMapper[T]](x: LongKeyedMetaMapper[T])(xs: List[String]) : Box[T] = idParser(xs) flatMap x.find


  def sitemap = SiteMap(
    Menu.i("Home")       / "index"
  , Menu.i("Activities") / "activities"
  // The activity page
  , Menu.params[Activity]("Activity", "activivity", objParser(Activity) _, idEncoder _)
        / "activity" / "view"
        >> IfValue({_ map {_.isViewable} openOr false}, S ? "No access")
        >> User.AddUserMenusAfter
        >> Hidden
  , Menu.i("Synchronize Activies") / "activity" / "synchronize" >> Hidden
    submenus (
        Menu.i("Synchronize Device") / "activity" / "sync"
      , Menu.i("add")                / "activity" / "add"
    )
  )
}
