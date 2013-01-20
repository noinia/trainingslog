package net.fstaals.tl.util

object BiMap {
  private[BiMap] trait MethodDistinctor
  implicit final object MethodDistinctor extends MethodDistinctor

  def empty[X,Y] = new BiMap[X,Y](Map.empty[X,Y])

}

// We require reverseMap to contain the same elements as map.
// This is not explicitly checked
class BiMap[X, Y](val map: Map[X, Y], private val reverseMap : Map[Y,X]) {

  def this(map: Map[X,Y]) = this(map, map map (_.swap))

  def this(tuples: (X,Y)*) = this(tuples.toMap)

  require(map.size == reverseMap.size, "no 1 to 1 relation")

  def apply(x: X): Y = map(x)
  def apply(y: Y)(implicit d: BiMap.MethodDistinctor): X = reverseMap(y)

  val domain = map.keys
  val codomain = reverseMap.keys

  def +(t: (X,Y)) = new BiMap(map + t, reverseMap + t.swap)

  def filter(p: ((X,Y)) => Boolean) = new BiMap(map filter p)

}
