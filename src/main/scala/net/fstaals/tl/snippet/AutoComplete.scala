package net.fstaals.tl.snippet


import scala.xml._

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import net.fstaals.tl.util._

import Helpers._

import js._
import JE._
import JsCmds._

import js.jquery._
import JqJE._


class TagSel {

  val all = (1 to 10) map (x => "Tag_" + x) toList

  def render = (new AutoComplete("addNew", List(), all) {
    override def newT(s: String) = Some(s)
  }).render

}

class AutoComplete[T](val htmlId : String
                     , current   : List[T]
                     , all       : List[T]) extends Html5AutoComplete[T](current,all) {


  def render =
    "*" #> (Templates(List("templates-hidden","autocomplete")) map selectors) &
    "*" #> PrefixId(htmlId)
}


class Html5AutoComplete[T]( var current  : List[T]
                          , var all      : List[T]) {

  def stringRep(x: T) : String = x.toString

  def available = all diff current

  def availableS = available map stringRep

  // the values whose string rep matches s
  def matching(s: String) = availableS filter {_.toLowerCase.startsWith(s)}

  // render the list with available options
  def dataList : NodeSeq = {
    def entry(s: String) : NodeSeq = <option value={s}/>

    val l = <datalist id="list">
             { availableS map entry }
            </datalist>
    l
  }

  // The element entered is not one of all, so generate a new elemnt
  // to be used.
  def newT(s: String) : Option[T] = None

  // Action to perform when adding this element to the list
  def add(x: T) : Unit = {
  }

  // helper function to run when we really have to add an element
  private def addElem(x: T) = {
    add(x)
    current ::= x
  }

  // function to execute when the user hits submit the current value
  def onSubmit(ac: IdMemoizeTransform)(s: String) : JsCmd = {
    available zip availableS filter {_._2.equalsIgnoreCase(s)} match {
        case ((x,_)::_) => addElem(x)
        case Nil        => newT(s) filterNot {(all ++ current) contains _} match {
          case Some(x)  => addElem(x)
          case None     => S.error("Invalid choice.")
        }
    }

    // refresh the rendering thingy
    ac.setHtml
  }

  val selectors = SHtml.idMemoize { ac => {

    val jsCalcValue = JqId("input") ~> JsFunc("val")

    "#button [onclick]" #> SHtml.ajaxCall(jsCalcValue, onSubmit(ac) _) &
    "#list" #> dataList  &
    "li *"              #> (current map stringRep)
  }}



}
