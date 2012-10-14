package net.fstaals.tl.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import Helpers._


import net.fstaals.tl.model._


object AddActivity {

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

  def add(af : ActivityFile) {
    S.notice("Path: " + af.toString)
    S.redirectTo("/")
  }

}





// class AddActivity extends UserSnippet {

//   var path = ""

//   def add = withUser (_add _)

//   def _add(u: User, xhtml : NodeSeq) : NodeSeq = {
//     def process() {

//     }


//     val af = load()

//     bind("activity",xhtml,
//          "raw" -> <span>{af.toString}</span>
//        )
//   }




//   def load() {

//     val af = ActivityFile(path)
//     af.load()

//     // val a = Activity.create
//     af
//   }

// }
