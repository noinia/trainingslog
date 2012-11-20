package net.fstaals.tl.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

class User extends MegaProtoUser[User] {
  def getSingleton = User

  /* ********** The fields Stored together with a User  ***************** */



  /* ********** The properties referenced from a User  ***************** */

  def activities = Activity.findAll(By(Activity.owner, this.id.get))

  def hrZones = HRZone.findAll(By(HRZone.owner,this.id.get))


  /* ********** other methods  ***************** */

  def fullName = firstName.get ++ " " ++ lastName.get

}

object User extends User with MetaMegaProtoUser[User] {
  override def dbTableName = "users"

  override def screenWrap = Full(<lift:surround with="default" at="content">
                                 <lift:bind /></lift:surround>)

  // define the order fields will appear in forms and output
  override def fieldOrder = List(id, firstName, lastName, email,
                                 locale, timezone, password)

  // comment this line out to require email validations
  override def skipEmailValidation = true
}


class HRZone extends LongKeyedMapper[HRZone] with IdPK {
  def getSingleton = HRZone

  /* ********** The fields Stored together with an Activity  ***************** */

  // Define a many-to-one (foreign key) relationship to the User class
  object owner            extends MappedLongForeignKey(this, User) {
    // add a database index for this column.
    override def dbIndexed_? = true
  }
  object lowerLimit extends MappedInt(this)
  object upperLimit extends MappedInt(this)
  object name       extends MappedString(this,50)
  object color      extends MappedString(this,6)

  override def validate = {
    val err = FieldError(lowerLimit,"HRZone needs: lowerLimit <= upperLimit")
    (if (lowerLimit.get <= upperLimit.get) Nil else List(err)) ++ super.validate
  }
}


object HRZone extends HRZone with LongKeyedMetaMapper[HRZone] {

  def newZone = User.currentUser map {u => HRZone.create.owner(u)}

}
