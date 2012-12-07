package net.fstaals.tl.widgets

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._


import js._
import JE._
import JsCmds._

import js.jquery._
import JqJE._


abstract class AutoCompleteSelector[T]( selector    : String
                                      , var current : List[T]
                                      , var all     : List[T]) {


  def stringRep(x: T) : String = x.toString

  def available = all diff current

  def availableS = available map stringRep

  // the values whose string rep matches s
  def matching(s: String) = availableS filter {_.toLowerCase.startsWith(s)}

  // The element entered is not one of all, so generate a new elemnt
  // to be used.
  def newT(s: String) : Option[T] = None

  // Action to perform when adding this element to the list
  def add(x: T) : Unit = {}

  private def addElem(x: T) = {
    add(x)
    current ::= x
  }

  // function to execute when the user hits enter to submit the current value
  def onEnter(s: String) : JsCmd = {
    available zip availableS filter {_._2.equalsIgnoreCase(s)} match {
        case ((x,_)::_) => addElem(x)
        case Nil        => newT(s) match {
          case Some(x)  => addElem(x)
          case None     => S.error("Invalid choice.")
        }
    }

    JsRaw("""$(%s).val("")""".format(selector))
  }

  def suggest : JsCmd = Call("autocomplete",selector, JsArray(availableS map Str))

  def render = {
    println(all)
    println(current)
    println(available)

    val jsOnEnter = SHtml.ajaxCall(JsRaw("""$(%s).val()""".format(selector)),
                                   onEnter _)

    // val jsSuggest = SHtml.ajaxCall(JsRaw("this.value"), suggest _)
    val js = JsIf(JsRaw("event.keyCode == 13"),jsOnEnter)

    "script *"                      #> suggest &
    "%s [onkeyup]".format(selector) #> js
  }

}
