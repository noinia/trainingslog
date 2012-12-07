package net.fstaals.tl.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import Helpers._

import net.fstaals.tl.model._

class ActivityList extends UserSnippet {


  def visibleActivities = Activity.publicActivities ++ Activity.myActivities

  def activities = "tr *" #> (visibleActivities map activity)


  def activity(a: Activity) =
    "a .name *"      #> a.name.get                           &
    "a .name [href]" #> "/activity/view/%d".format(a.id.get) &
    ".date *"        #> a.start.get.toString                 &
    ".duration *"    #> HhMmSs(a.duration.get)               &
    ".distance *"    #> Km(a.distance)


  // def activities(xhtml : NodeSeq) : NodeSeq = User.currentUser match {
  //   case Full(user) => {
  //     val entries : NodeSeq = user.allActivities match {
  //       case Nil  => Text("No activities")
  //       case acts => acts flatMap { a =>
  //                           bind("activity",xhtml,
  //                                "entry" -> Text(a.name.is)
  //                               ) }
  //       }
  //     bind("activity", xhtml, "entry" -> entries)
  //   }
  //   case _          => <h1>test</h1>
  // }

}
