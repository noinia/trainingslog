package net.fstaals.tl.snippet

import scala.xml._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import js._
import JE._
import JsCmds._

import js.jquery._
import JqJE._

class Button( val text       : String
            , val id         : String
            , val buttonType : String = "regular"
            , val action     : JsCmd  = Noop
            , val img        : String = "/images/buttons/accept.png") {


  def selectors = ".text"                    #> text             &
                  "#button [class+]"         #> buttonType       &
                  "#button [onclick]"        #> action.toJsCmd   &
                  "img [src]"                #> img              &
                  "type=hidden [id]"         #> (id ++ "hidden") &
                  "#button [id]"             #> id

  def render = "*" #> (Templates(List("templates-hidden","button")) map selectors)


}

object Button {

}

object AcceptButton {

  def apply(s: String, id: String) = new Button(s,id,"accept")
}
