package net.fstaals.tl.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import Helpers._

import net.fstaals.tl.model._

class ActivityList extends UserSnippet {

  def activities = withUser (_activities _)

  def _activities(u: User, xhtml : NodeSeq) : NodeSeq = {
    val entries : NodeSeq = u.allActivities match {
      case Nil  => Text("No activities")
      case acts => acts flatMap { a =>
        bind("activity",xhtml,
             "name" -> <li>{a.name.is}</li>
           ) }
    }
    bind("activity", xhtml, "entry" -> entries)
  }

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
