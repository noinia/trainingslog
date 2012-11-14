package net.fstaals.tl.snippet

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import net.fstaals.tl.model._



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
