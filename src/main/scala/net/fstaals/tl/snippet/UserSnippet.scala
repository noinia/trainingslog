package net.fstaals.tl.snippet

import net.liftweb.common._
import net.liftweb.mapper._
import scala.xml.{NodeSeq, Text}
import net.liftweb.http._


import net.fstaals.tl.model._
import net.fstaals.tl._

trait UserSnippet {

  type NSTrans = (NodeSeq => NodeSeq)

  def login(x : NodeSeq) = <h1>Not logged in</h1>


  def noUserMsg = S.notice("error not logged in")

  def withUser(f: (User,NodeSeq) => NodeSeq) : NSTrans = withUserOr(f)(login _)

  def withUserOr(f: (User,NodeSeq) => NodeSeq)(g: NSTrans) : NSTrans =
    User.currentUser match {
      case Full(user) => f(user,_)
      case _          => g
    }

}


// abstract class ModelSnippet[T <: LongKeyedMapper[T]](val pk : PK) {

//   def getModel : T = {
//       singleton.find(pk.id) match {
//         case Full(model) if canAccess(model) =>
//           model
//         case Full(_)                         =>
//           S.redirectTo("/noaccess")
//         case _                               =>
//           S.redirectTo("/nosuch")
//       }

//   }

//   def canAccess(x : T) : Boolean
//   def singleton : LongKeyedMetaMapper[T]

// }
