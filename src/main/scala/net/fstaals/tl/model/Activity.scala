package net.fstaals.tl.model

import org.scala_tools.time.Imports._
import net.liftweb.common._
import scala.xml._
import net.liftweb.mapper._

class Activity extends LongKeyedMapper[Activity] with IdPK with ManyToMany {

  def getSingleton = Activity


  /* ********** The fields Stored together with an Activity  ***************** */

  // Define a many-to-one (foreign key) relationship to the User class
  object owner            extends MappedLongForeignKey(this, User) {
    // add a database index for this column.
    override def dbIndexed_? = true
  }
  object isPublic         extends MappedBoolean(this) {
    override def defaultValue = false
  }
  object name              extends MappedString(this,200)
  object start             extends MappedDateTime(this)
  object end               extends MappedDateTime(this)
  object activityFilePath  extends MappedString(this,500)
  //TODO: make this into a separate table/class. since most activities
  // will not have a description:
  object description       extends MappedTextarea(this, 2048) {
    override def textareaRows  = 10
    override def textareaCols  = 50
  }

  /* ********** The properties referenced from an Activity  ***************** */

  // fields expressed by relations:
  def exercises = Exercise.findAll(By(Exercise.activity, this.id))
  // def sport     = Sport.getBy
  // def category

  // tags is many to many
  object tags              extends MappedManyToMany(ActivityTags, ActivityTags.activity,
                                                    ActivityTags.tag, Tag)

  /* ********** other methods  ***************** */

  lazy val af = ActivityFile.fromPath(activityFilePath.get)

  def duration    = af flatMap {_.duration}
  def speed       = af flatMap {_.speed}
  def distance    = af flatMap {_.distance}
  def cadence     = af flatMap {_.cadence}
  def temperature = af flatMap {_.temperature}


}

object Activity extends Activity with LongKeyedMetaMapper[Activity] {

  implicit def jodaDTtoDT(d : DateTime) = d.toDate()
  implicit def dtToJodaDt(d: java.util.Date) = new DateTime(d)

  def fromActivityFile(af: ActivityFile) = create.activityFilePath(af.path)
                                                 .start(af.start.getOrElse(DateTime.now))
                                                 .end(af.end.getOrElse(DateTime.now))

}

class Exercise extends LongKeyedMapper[Exercise] with IdPK {

  def getSingleton = Exercise


  /* ********** The fields Stored together with an Exercise  ***************** */

  object name              extends MappedString(this,200)
  object start             extends MappedDateTime(this)
  object end               extends MappedDateTime(this)
  object rpe               extends MappedInt(this)
  object description       extends MappedTextarea(this, 2048) {
    override def textareaRows  = 10
    override def textareaCols  = 50
  }

  /* ********** The properties referenced from an Exercise  ***************** */

  // Define a many-to-one (foreign key) relationship to the Activity class
  object activity          extends MappedLongForeignKey(this, Activity) {
    // add a database index for this column.
    override def dbIndexed_? = true
  }

  // fields expressed by relations:
  // def PlannedExercise


  /* ********** other methods  ***************** */

}

object Exercise extends Exercise with LongKeyedMetaMapper[Exercise]


// class Exercise( val name : String
//               , var rpe  : Option[UserProfile.RPE]
//               , val description : String
//               , val start : DateTime
//               , val end   : DateTime)
