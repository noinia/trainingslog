package net.fstaals.tl.util

import scala.xml._
import scala.xml.transform._

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._


object ChangeAttr {
  /**  Helper object for manipulating attributes. I.e. we change something like:
   *
   *   <foo id="foo"><bar id="bar"><baz>a</baz></bar></foo>
   *
   *   to
   *
   *   <foo id="foo"><bar id="bar"><baz>a</baz></bar></foo>
   */

  // Stuff for manipulating attributes

  case class GenAttr(pre: Option[String],
                     key: String,
                     value: Seq[Node],
                     next: MetaData) {
    def toMetaData = Attribute(pre, key, value, next)
  }

  def decomposeMetaData(m: MetaData): Option[GenAttr] = m match {
    case Null => None
    case PrefixedAttribute(pre, key, value, next) =>
      Some(GenAttr(Some(pre), key, value, next))
    case UnprefixedAttribute(key, value, next) =>
      Some(GenAttr(None, key, value, next))
  }

  def unchainMetaData(m: MetaData): Iterable[GenAttr] =
    m flatMap (decomposeMetaData)

  def chainMetaData(l: Iterable[GenAttr]): MetaData = l match {
    case Nil => Null
    case head :: tail => head.copy(next = chainMetaData(tail)).toMetaData
  }

  // map stuff over the meta data
  def mapMetaData(m: MetaData)(f: GenAttr => GenAttr): MetaData =
    chainMetaData(unchainMetaData(m).map(f))

  // lift the function that changes the id into a genAttr
  def liftGenAttr(key: String)(f: String => String)(g: GenAttr) = g match {
    case GenAttr(_, k, Text(v), _) if k == key => g.copy(value = Text(f(v)))
    case _                                     => g
  }

  // We builld a CSS Selector That manipulates the xml elements
  def apply(attrName: String)(f: String => String) = new CssSel {
    def apply(xhtml: NodeSeq) = {
      val rr = new RewriteRule() {
        override def transform(n: Node): Seq[Node] = (n.attribute("id"),n) match {
                                  // if we do not have an id simply use the identity
          case (None,_)           => n
                                  // if we have an id, update it, and recursively
                                  // process it's children
          case (Some(i),e : Elem) =>
            e.copy( attributes =
                      mapMetaData(e.attributes)(liftGenAttr(attrName)(f) _)
                  , child      = e.child flatMap transform
                  )
                                  // apparently we can also have different types of
                                  // nodes. Ignore those
          case _                  => n
        }
      }
      xhtml map rr
    }
  }

}
