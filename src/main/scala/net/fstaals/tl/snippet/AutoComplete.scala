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

  def render = (new Html5AutoComplete("foo", List(), all) {
    override def newT(s: String) = Some(s)
  }).render

}

class Html5AutoComplete[T]( val prefixId : String
                          , var current  : List[T]
                          , var all      : List[T]) {


  // The element entered is not one of all, so generate a new elemnt
  // to be used.
  def newT(s: String) : Option[T] = None

  // Action to perform when adding this element to the list
  def add(x: T) : Unit = { }

  // Action to perform when deleting this element from the list
  def del(x: T) : Unit = { }

  def stringRep(x: T) : String = x.toString


  // --------------------------------------------------------------------

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

  // helper function to run when we really have to add an element
  private def addElem(x: T) = {
    add(x)
    current ::= x
  }

  private def delElem(x: T) = {
    current = current filter {_ != x}
    del(x)
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

  def onDelete(ac: IdMemoizeTransform)(s: String) : JsCmd = {

    // refresh everything again
    ac.setHtml
  }

  def render = "*" #> (Templates(List("templates-hidden","autocomplete")) map selectors)

  val selectors = SHtml.idMemoize { ac => {

    val jsCalcValue = JqId(prefix("input")) ~> JsFunc("val")

    "#button [onclick]" #> SHtml.ajaxCall(jsCalcValue, onSubmit(ac) _) &
    "#list"             #> dataList                                    &
    ".item *"           #> (current map {".value *" #> stringRep(_)})  &
    prefixIds
  }}


  // prefixes the ids that should be prefixed (and removes the prefix attribute)
  def prefixIds = "prefixid=true"             #> ChangeAttr("id")(prefix _)    &
                  "list=list"                 #> ChangeAttr("list")(prefix _)  &
                  "prefixid=true [prefixid!]" #> ""


  private def prefix(s: String) = prefixId + s



}
