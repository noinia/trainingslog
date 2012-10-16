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

  def sitemap = SiteMap(
    Menu.i("Home")       / "index"
  , Menu.i("Activities") / "activities"
  // Handling an activity
  , Menu(ModelParamLoc[Activity]("Activity",
                                 new Link(List("activivity")),
                                 "Activity",
                                 List("activity"),
                                 Activity,
                                 activityIsViewable _,
                                 List(User.AddUserMenusAfter, Hidden)),
          Menu.i("view/edit") / "activity"
        , Menu.i("Synchronize Device") / "activity" / "sync"
        , Menu.i("add")                / "activity" / "add"
    )

  // Error pages
  , Menu.i("nosuch")   / "nosuch"    >> Hidden //TODO, see if I can use noSuchPathPath
                                               // to define this
  , Menu.i("noaccess") / "noaccess"  >> Hidden
  )

  val noSuchPath   = List("nosuch")
  val noAccessPath = List("noaccess")

  // Access control stuff
  def activityIsViewable(a: Activity) =
    a.isPublic.get || Full(a.owner.get) == User.currentUser

}


// For parameters that depend on stuff in the database
abstract class ModelParam[+T]
case object NoSuch                    extends ModelParam[Nothing]
case object NoAccess                  extends ModelParam[Nothing]
case class FullModel[T](val model: T) extends ModelParam[T]




case class ModelParamLoc[T <: LongKeyedMapper[T]]( val name: String
                                                 , val link: Link[ModelParam[T]]
                                                 , val text: LinkText[ModelParam[T]]
                                                 , val path: List[String]
                                                 , val singleton : LongKeyedMetaMapper[T]
                                                 , val canAccess : T => Boolean =
                                                   {x : T => true}
                                                 , val params : List[LocParam[ModelParam[T]]]) extends Loc[ModelParam[T]]  {


  val defaultValue = Empty

  // match this LOC matches the path if the rqPath is exactly
  // path + <id>
  def matchIDPath(rqPath : List[String]) =
    rqPath.nonEmpty && rqPath.init == path && (rqPath.lastOption flatMap asLong).isDefined

  override def rewrite = Full({
    case RewriteRequest(ParsePath(rqpath, _, _, _), _, _)  if matchIDPath(rqpath) => {
      val id   = rqpath.last

      singleton.find(id) match {
        case Full(model) if canAccess(model) =>
          (RewriteResponse(path), FullModel(model))
        case Full(_)                         =>
          (RewriteResponse(TLSiteMap.noAccessPath), NoAccess)
        case _                               =>
          (RewriteResponse(TLSiteMap.noSuchPath), NoSuch)
      }
    }})

}
