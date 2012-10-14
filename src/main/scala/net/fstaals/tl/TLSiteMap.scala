package net.fstaals.tl

import net.liftweb._
import sitemap._
import Loc._

import net.fstaals.tl.model._

object TLSiteMap {
  def sitemap = SiteMap(
    Menu.i("Home") / "index"
  , Menu.i("Activities") / "activities"
  , Menu.i("Activity") / "activity" / "index" >> User.AddUserMenusAfter submenus (
      Menu.i("add") / "activity"/ "add"
    )
    // more complex because this menu allows anything in the
    // /static path to be visible
  // , Menu(Loc("Static", Link(List("static"), true, "/static/index"), "Static Content"))
  )


}
