package net.fstaals.model

import net.liftweb.mapper._

case class Color(val name: String)

class Tag extends LongKeyedMapper[Tag] with IdPK with ManyToMany {
  def getSingleton = Tag

  object tag  extends MappedString(this, 20)

  object activities extends MappedManyToMany(ActivityTags, ActivityTags.tag,
                                             ActivityTags.activity, Activity)

}

object Tag extends Tag with LongKeyedMetaMapper[Tag]

// The relation between activities and Tags

class ActivityTags extends LongKeyedMapper[ActivityTags] with IdPK {
  def getSingleton = ActivityTags
  object activity extends MappedLongForeignKey(this, Activity)
  object tag      extends MappedLongForeignKey(this, Tag)
}

object ActivityTags extends ActivityTags with LongKeyedMetaMapper[ActivityTags]


class Sport extends LongKeyedMapper[Sport] with IdPK {
  def getSingleton = Sport

  object name extends MappedString(this,200)
}

object Sport extends Sport with LongKeyedMetaMapper[Sport]
