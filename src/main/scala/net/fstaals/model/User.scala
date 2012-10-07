package net.fstaals.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

class User extends MegaProtoUser[User] {
  def getSingleton = User

  /* ********** The fields Stored together with a User  ***************** */



  /* ********** The properties referenced from a User  ***************** */

  def allActivities = Activity.findAll(By(Activity.owner, this.id))


  /* ********** other methods  ***************** */


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
