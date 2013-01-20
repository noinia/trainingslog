package net.fstaals.tl.snippet

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._

import net.fstaals.tl.model._


import Helpers._

class Tags extends StatefulSnippet {

  def dispatch = {
    case _ => render
  }

  def render = {
    val allTags = Tag.myTags

    (new TagSelector(allTags,allTags) {
      override def add(t: Tag) = t.validate match {
        case Nil => { t.save ; S.notice("Saved")}
        case xs  => S.error(xs)
      }

      override def del(t: Tag) = {
        t.delete_!
        // TODO: Maybe we have to  cleanup the tag mapping tables as well
        S.notice("Tag "+stringRep(t)+" deleted.")
      }

    }).render

  }

}

class TagSelector( currentTags : List[Tag]
                 , allTags     : List[Tag] = Tag.myTags) extends
      Html5AutoComplete[Tag]("addTag", currentTags, allTags) {

  override def stringRep(x: Tag) = x.tag.get

  override def newT(s: String) = Tag(s)

  def renderReadOnly = "*" #> <ul>
                                { currentTags map {t => <li>{stringRep(t)}</li>} }
                              </ul>


}
