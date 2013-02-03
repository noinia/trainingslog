package net.fstaals.tl.model

import net.liftweb.mapper._
import net.liftweb.common._


trait HasOwner[T <: HasOwner[T]] extends Mapper[T] {

 self: T =>

  object owner  extends MappedLongForeignKey[T,User](this, User) {
    // add a database index for this column.
    override def dbNotNull_?  = true
  }

}




class Tag extends LongKeyedMapper[Tag] with IdPK with ManyToMany with HasOwner[Tag] {
  def getSingleton = Tag

  object tag    extends MappedString(this, 20) {
    override def dbIndexed_? = true
  }

  object activities extends MappedManyToMany(ActivityTags, ActivityTags.tag,
                                             ActivityTags.activity, Activity)


  override def equals(o: Any) = o.isInstanceOf[Tag] &&
    { val t = o.asInstanceOf[Tag] ; t.tag.get == tag.get && t.owner.get == owner.get }

}

object Tag extends Tag with LongKeyedMetaMapper[Tag] {

  override def dbIndexes = UniqueIndex(owner,tag) :: super.dbIndexes

  def apply(st: String) : Box[Tag] = (st.trim,User.currentUser) match {
    case ("",_)       => Failure("Tag cannot be empty.")
    case (s ,Full(u)) => Full(create.tag(s).owner(u))
    case (_,bu)       => Failure("Error getting current user.")
  }


  def fromString(s: String) : List[Tag] = s.split(",").toList flatMap apply

  def showTags(ts: Seq[Tag]) = ts mkString ", "

  def myTags = User.currentUser.toList flatMap {u => findAll(By(owner,u))}

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
