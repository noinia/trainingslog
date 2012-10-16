package net.fstaals.tl.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import Helpers._
import net.fstaals.tl.model._
import net.fstaals.tl._


class ActivitySnippet(model : ModelParam[Activity])
      extends ModelSnippet[Activity](model) with UserSnippet with StatefulSnippet {

  lazy val activity = getModel

  def dispatch = {
    case "summary" => editSummary
    case "graphs"  => graphs
    case "map"     => map
    case "details" => details
  }


  def title = "#activityName *" #> "foo"

  // def summary = "#activityName *" #> "foo"

  def editSummary = "#summary" #> Activity.toForm(activity)

  def graphs = "#activityName *" #> "foo"

  def map = "#activityName *" #> "foo"

  def details = "#activityName *" #> "foo"

}
