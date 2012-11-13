package net.fstaals.tl.model

import net.liftweb.mapper._

case class Color(val name: String)

class Tag extends LongKeyedMapper[Tag] with IdPK with ManyToMany  {
  def getSingleton = Tag

  object tag  extends MappedString(this, 20)

  object activities extends MappedManyToMany(ActivityTags, ActivityTags.tag,
                                             ActivityTags.activity, Activity)

}

object Tag extends Tag with LongKeyedMetaMapper[Tag] {

  def apply(s: String) = create.tag(s)

  def fromString(s: String) : List[Tag] = s.split(" ").toList map apply

  def showTags(ts: Seq[Tag]) = ts mkString ", "

}

// The relation between activities and Tags

class ActivityTags extends LongKeyedMapper[ActivityTags] with IdPK {
  def getSingleton = ActivityTags
  object activity extends MappedLongForeignKey(this, Activity)
  object tag      extends MappedLongForeignKey(this, Tag)
}

object ActivityTags extends ActivityTags with LongKeyedMetaMapper[ActivityTags] {

  def tag(a: Activity, t: Tag) = create.activity(a).tag(t).save


}


class Sport extends LongKeyedMapper[Sport] with IdPK {
  def getSingleton = Sport

  object name extends MappedString(this,200)
}

object Sport extends Sport with LongKeyedMetaMapper[Sport]
