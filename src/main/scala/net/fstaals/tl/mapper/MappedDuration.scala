package net.fstaals.tl.mapper


import com.github.nscala_time.time.Imports._
import org.joda.time.Duration

import java.sql.Types
import java.lang.reflect.Method

import net.liftweb.mapper._

import net.fstaals.tl.model.HhMmSs
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import scala.xml.NodeSeq
import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js._
import net.liftweb.json._


abstract class MappedDuration[T <: Mapper[T]](val fieldOwner: T) extends MappedField[Duration,T] {

  private var data    : Duration = defaultValue
  private var orgData : Duration = defaultValue

  val defaultValue : Duration = Duration.ZERO
  def dbFieldClass = classOf[Duration]

  /* We store a duration as a Long in the database. So many of these methods
   * are simply taken/adapted from MappedLong
   */
  def targetSQLType = Types.BIGINT

  protected def i_is_!  = data
  protected def i_was_! = orgData

  /**
* Called after the field is saved to the database
*/
  override def doneWithSave() {
    orgData = data
  }

  protected def real_i_set_!(value : Duration): Duration = {
    if (value != data) {
      data = value
      dirty_?(true)
    }
    data
  }

  protected def i_obscure_!(in : Duration) = defaultValue

  override def readPermission_? = true
  override def writePermission_? = true

  override def setFromAny(in: Any): Duration = {
    in match {
      case d: Duration              => this.set(d)
      case n: Long                  => this.set(toDuration(n))
      case JsonAST.JInt(bigint)     => this.set(toDuration(bigint.longValue))
      case n: Number                => this.set(toDuration(n.longValue))
      case (n: Number) :: _         => this.set(toDuration(n.longValue))
      case Some(n: Number)          => this.set(toDuration(n.longValue))
      case Full(n: Number)          => this.set(toDuration(n.longValue))
      case s: String                => this.set(toDuration(s))
      case (s: String) :: _         => this.set(toDuration(s))
      case x :: _                   => this.setFromAny(x)
      case Empty | Failure(_, _, _) => this.set(defaultValue)
      case None                     => this.set(defaultValue)
      case null                     => this.set(defaultValue)
      case o                        => this.set(toDuration(o))
    }
  }

  private def setFromLong(x : Long) = this.set(toDuration(x))

  private def toDuration(s : String) : Duration = HhMmSs.parse(s) match {
    case Some(d) => d
    case _       => defaultValue
  }
  private def toDuration(x : Long)   : Duration = new Duration(x)
  private def toDuration(v : Any)    : Duration = toDuration(v.toString.toLong)



  private def st(in: Duration) {
    data = in
    orgData = in
  }

  def asJsExp: JsExp = JE.Num(is.millis)

  def asJsonValue: Box[JsonAST.JValue] = Full(JsonAST.JInt(is.millis))


  def real_convertToJDBCFriendly(value: Duration): Object = new java.lang.Long(value.millis)


  def jdbcFriendly(field : String) = new java.lang.Long(i_is_!.millis)
  override def jdbcFriendly = new java.lang.Long(i_is_!.millis)


  // Build the stuff we set in the database

  def buildSetActualValue(accessor: Method, data: AnyRef, columnName: String) : (T, AnyRef) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedDuration[T] => f.st(toDuration(v))})

  def buildSetLongValue(accessor: Method, columnName : String) : (T, Long, Boolean) => Unit =
  (inst, v, isNull) => doField(inst, accessor, {case f: MappedDuration[T] => f.st(if (isNull) defaultValue else toDuration(v))})

  def buildSetStringValue(accessor: Method, columnName: String): (T, String) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedDuration[T] => f.st(toDuration(v))})

  def buildSetDateValue(accessor : Method, columnName : String) : (T, Date) => Unit =
  (inst, v) => doField(inst, accessor, {case f: MappedDuration[T] => f.st(if (v == null) defaultValue else toDuration(v.getTime))})

  def buildSetBooleanValue(accessor : Method, columnName : String) : (T, Boolean, Boolean) => Unit = null

  /**
   * Given the driver type, return the string required to create the column in the database
   */
  def fieldCreatorString(dbType: DriverType, colName: String): String = colName + " " + dbType.longColumnType + notNullAppender()

}
