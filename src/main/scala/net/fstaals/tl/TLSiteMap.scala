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

case class PK(id: Long)

object TLSiteMap {

  def idParser(xs: List[String]) = try {
    Full(PK(xs.head.toLong))
  } catch {case _ => Empty}
  def idEncoder(k: PK)  = List(k.id.toString)

  def sitemap = SiteMap(
    Menu.i("Home")       / "index"
  , Menu.i("Activities") / "activities"
  // Handling an activity
  , Menu.params[PK]("Activity", "activivity",
                    idParser _, idEncoder _)
                    / "activity" / "view" >> User.AddUserMenusAfter >> Hidden submenus (
          Menu.i("Synchronize Device") / "activity" / "sync"
        , Menu.i("add")                / "activity" / "add"
    )

  // Error pages
  , Menu.i("nosuch")   / "nosuch"    >> Hidden //TODO, see if I can use noSuchPathPath
                                               // to define this
  , Menu.i("noaccess") / "noaccess"  >> Hidden
  )

  val noSuchPath   = List("nosuch")
  val noAccessPath = List("noaccess")

}


object AccessControl {
  def activityIsViewable(a: Activity) =
    a.isPublic.get || (User.currentUser map {_.id}) === a.owner.get

}
