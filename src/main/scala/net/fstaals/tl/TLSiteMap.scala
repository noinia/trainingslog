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
  , Menu.i("User") / ""
      >> PlaceHolder
      >> User.AddUserMenusUnder
  , Menu.i("Activity") / "activity" / "index"
         submenus (
           Menu.params[Activity]("View", "View",
                                objParser(Activity) _, idEncoder _)
            / "activity" / "view"
            >> IfValue({_ map {_.isViewable} openOr false}, S ? "No access")
         , Menu.params[Activity]("Graphs", "Graphs",
                              objParser(Activity) _, idEncoder _)
          / "activity" / "graphs"
          >> IfValue({_ map {_.isViewable} openOr false}, S ? "No access")
    )
  , Menu.i("Synchronize Device") / "activity" / "sync" submenus (
        Menu.i("Add")                / "activity" / "add"
    )
  , Menu.i("Tags")  / "tags"
  , Menu.i("HR Zones") / "hrzones"
  )
}
