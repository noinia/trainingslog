package net.fstaals.tl.snippet

import net.liftweb.common._
import scala.xml.{NodeSeq, Text}

import net.fstaals.tl.model._

trait UserSnippet {

  type NSTrans = (NodeSeq => NodeSeq)

  def login(x : NodeSeq) = <h1>Not logged in</h1>

  def withUser(f: (User,NodeSeq) => NodeSeq) : NSTrans = withUserOr(f)(login _)

  def withUserOr(f: (User,NodeSeq) => NodeSeq)(g: NSTrans) : NSTrans =
    User.currentUser match {
      case Full(user) => f(user,_)
      case _          => g
    }

}