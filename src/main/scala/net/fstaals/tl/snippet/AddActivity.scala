package net.fstaals.tl.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import Helpers._


import net.fstaals.tl.model._


object SyncActivities extends UserSnippet {

  def render = {
    var af : ActivityFile = ActivityFile("dummy")

    def load(path: String) {
      af = ActivityFile(path)
      S.notice("Path: "+path)
      af.load()
    }

    def process() = {
      if (af.hasData)
        add(af)
      else {
        S.notice("Failed to load ActivityFile")
      }
    }

    "name=path"   #> SHtml.onSubmit(load) &
    "type=submit" #> SHtml.onSubmitUnit(process)
  }

  def add(af : ActivityFile) = User.currentUser match {
    case Full(u) => {
      val a = Activity.fromActivityFile(af).owner(u)

      S.redirectTo("/")
    }
    case _       => noUserMsg
  }

}
