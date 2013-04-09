package net.fstaals.tl.mapper

import net.fstaals.tl.util._

import net.liftweb.mapper._

import net.liftweb.util._
import net.liftweb.common._


class MappedColor[T <: Mapper[T]](towner: T) extends MappedString[T](towner, 6) {

  override def is = "#" ++ super.is

  override def setFilter = dropHash _ :: super.setFilter

  def dropHash(s: String) = if (s != "" && s.charAt(0) == '#') s.substring(1) else s

  override def validate = isColor(get.substring(1)) ++: super.validate

  def isColor(s: String) = s.toList.grouped(2).toList match {
    case a :: b :: c :: Nil if isHex(a) && isHex(b) && isHex(c) => None
    case _                                               =>
      Some(FieldError(this,"Not a valid hexadecimal color"))
  }

  def isHex(s: List[Char]) =
    ConversionHelpers.opt(Integer.parseInt(s.mkString(""),16)) nonEmpty


}
