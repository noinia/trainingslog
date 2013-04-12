package net.fstaals.tl.util

import java.awt.Color

import scala.util.Try


object ConversionHelpers {
  def opt[T](t : => T) = Try(t).toOption
}

object ColorUtil {

  def toAWTColor(color: String) = {
    val xs : List[Int] = color.toList.tail.grouped(2).toList flatMap {x =>
      Try(Integer.parseInt(x.mkString(""),16)).toOption }
    xs match {
      case r :: g :: b :: Nil => Try(new Color(r,g,b)).toOption
      case _                  => None
  }}

  def toHex(c: Color) = {
    def f(x: Int) = {
      val y = "0" + Integer.toHexString(x)
      y.substring(y.length - 2)
    }

    val r = f(c.getRed())
    val g = f(c.getGreen())
    val b = f(c.getBlue())

    "#%s%s%s".format(r,g,b)
  }

  def lighten(cs: String, l: Double) = toAWTColor(cs) map {c =>
    val rgb = List(c.getRed(),c.getGreen(),c.getBlue())
    def f(x: Int) : Int = math.round((255 - x) * l.toFloat + x)

    val rgbp = rgb map f

    toHex(if (rgbp.max > 255) c else new Color(rgbp(0), rgbp(1), rgbp(2)))

    // val hsl     = RGBtoHSL(c)
    // println((hsl(0),hsl(1),hsl(2)))
    // toHex(new Color(HSLtoRGB(Array(hsl(0),hsl(1), 0.75.toFloat ))))
  }

  // lightness in [1,100]
  def toBGColor(c: String, lightness: Double) =
    lighten(c,lightness) getOrElse c

}
