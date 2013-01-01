package net.fstaals.tl.snippet

import scala.xml._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import Helpers._

import js._
import JE._
import JsCmds._

import js.jquery._
import JqJE._

class Button( val text       : NodeSeq
            , val id         : Option[String] = None
            , val buttonType : String         = Button.defaultType
            , val img        : String         = Button.defaultImg) {


  // def this(txt: String, id: String,
  //          buttonType: String = Button.defaultType,
  //          img: String        = Button.defaultImg) =
  //            this(Text(txt),id,buttonType,img)

  def selectors = ".text"                    #> text             &
                  "#button [class+]"         #> buttonType       &
                  "img [src]"                #> img              &
                  setIds

  def setIds = id map {i =>
      "type=hidden [id]"         #> (i ++ "hidden") &
       "#button [id]"            #> i} getOrElse
      // or else
      "type=hidden [id] ^^"      #> "*" &
      "#button [id] ^^"          #> "*"


  def render = "*" #> (Templates(List("templates-hidden","button")) map selectors)




}

trait MetaButton {

  def button(s: String, id: Option[String]) : Button = new Button(Text(s),id)
  def button(s: String, id: String)         : Button = button(s,Some(id))

  def render(x: NodeSeq) = {
    println(x)

    val s = x.head.child.text
    val i = x.head attribute "id" map {_.head text}

    button(s,i).render(x)
  }

}


object Button extends MetaButton {
  val defaultType = "regular"
  val defaultImg  = "/images/buttons/accept.png"

}

object AcceptButton extends MetaButton {
  override def button(s: String, id: Option[String]) = new Button(Text(s),id,"accept")
}

object CancelButton extends MetaButton {
  override def button(s: String, id: Option[String]) =
    new Button(Text(s),id,"cancel","/images/buttons/cancel.png")
}
