package net.fstaals.tl.snippet

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import net.fstaals.tl.model._
import net.fstaals.tl.widgets._

class Tags extends UserSnippet with StatefulSnippet {

  var currentTags : List[Tag] = Tag.myTags

  def dispatch = {
    case _ => render
  }

  def render = {
    var t : Box[Tag] = Empty

    def add() = t.toList flatMap {_.validate} match {
      case Nil => { t map {_.save} ; currentTags = (t ++ currentTags).toList }
      case xs  => S.error(xs)
    }

    "#newTag" #> SHtml.onSubmit(s => {t = Tag(s)}) &
    "#addTag" #> SHtml.onSubmitUnit(add)           &
    "li *"    #> (currentTags map {_.tag.get})
  }

}

class TagSelector( currentTags : List[Tag]
                 , allTags     : List[Tag] = Tag.myTags) extends
      AutoCompleteSelector[Tag](".addTag", currentTags, allTags, Some("li")) {

  override def stringRep(x: Tag) = x.tag.get

  override def newT(s: String) = Tag(s)

  def renderReadOnly = ".addTag" #> "" &
                       "script"  #> "" &
                       updateList

}
