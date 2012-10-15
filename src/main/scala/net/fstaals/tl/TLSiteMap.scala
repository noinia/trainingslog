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
  , Menu.param[IDParam]("Activity","Activity",asID, p => p.id.toString) / "activity"
  , Menu.i("Foo")   / "activity" / "flop" >> User.AddUserMenusAfter submenus (
      Menu.i("Synchronize Device") / "activity" / "sync"
    , Menu.i("add")                / "activity" / "add" >> Hidden
    )
    // more complex because this menu allows anything in the
    // /static path to be visible
  // , Menu(Loc("Static", Link(List("static"), true, "/static/index"), "Static Content"))
  )


  case class IDParam(id: Long)
  def asID(s: String) = asLong(s) map IDParam

  def ifInDB[T <: LongKeyedMapper[T]](singleton : LongKeyedMetaMapper[T]) = {

    def test(b: Box[IDParam]) : Boolean = b match {
      case Full(IDParam(id)) => singleton.find(id).isDefined
      case _                 => false
    }

    IfValue(test,S ? "Failed to find the requested object." )
  }

}


// For parameters that depend on stuff in the database
abstract class ModelParam[-T]
case object NoSuch                    extends ModelParam[Any]
case object NoAccess                  extends ModelParam[Any]
case class FullModel[T](val model: T) extends ModelParam[T]



case class ModelParamLoc[T <: LongKeyedMapper[T]]( val name: String
                                                 , val link: Link[ModelParam[T]]
                                                 , val text: LinkText[ModelParam[T]]
                                                 , val defaultValue : Box[ModelParam[T]]
                                                 , val singleton : LongKeyedMetaMapper[T]
                                                 , val canAccess : T => Boolean
                                                 , val params : List[LocParam[ModelParam[T]]]) extends Loc[ModelParam[T]]  {


  override def rewrite = Full({
    case RewriteRequest(ParsePath(List("activity", id), _, _, _), _, _) => {
      singleton.find(id) match {
        case Full(model) if canAccess(model) =>
          (RewriteResponse(List("activity")), FullModel(model))
        case Full(_)                         =>
          (RewriteResponse(List("activity")), NoAccess)
        case _                               =>
          (RewriteResponse(List("activity")), NoSuch)
      }
    }})



}





// class ModelParamLoc[T](val path       : List[String],
//                        val singleton  : LongKeyedMetaMapper[T],
//                        val accessable : T => Bool) extends Loc[ModelParam[T]] {
//   override def rewrite = Full({
//     case RewriteRequest(ParsePath(path + id, _, _, _), _, _) => {
//       singleton.findAll(By(singleton.stringId,id)) match {
//         case List(model) if accessable(model) => {
//         (RewriteResponse("account" :: Nil),
//          FullModel(model))
//         }
//         case List(_) => {
//           (RewriteResponse("account" :: Nil),
//            NoAccess)
//         }
//         case _ => {
//           (RewriteResponse("account" :: Nil),
//            NoSuch)
//         }
//     }
//   }})

// }




// // abstract class ModelParam[T <: LongKeyedMapper[T]](val km: LongKeyedMetaMapper[T])
// // case     class NoSuch[    T <: LongKeyedMapper[T]](km: LongKeyedMetaMapper[T]) extends ModelParam(km)



// // case class NoAccess[T](km: LongKeyedMetaMapper[T]) extends ModelParam(km)
// // case class FullModel[T <: LongKeyedMapper[T]](val model : T) extends ModelParam(model.getSingleton)

// // class ModelParamLoc[T <: LongKeyedMapper[T]](path: List[String]) extends Loc[ModelParam[T]] {

// //   override def rewrite = Full({
// //     case RewriteRequest(ParsePath(path + id, _, _, _), _, _) => {
// //       km.findAll()
// //     }
// //   })

// // }
