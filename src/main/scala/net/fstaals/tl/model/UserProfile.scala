package net.fstaals.tl.model

import org.scala_tools.time.Imports._

object UserProfile {
  type RPE = Int
}

class UserProfile {
  var hrZones    = List(HRZone("default",Int.MinValue,Int.MaxValue,Color("white")))
  val rpres      = 1 to 10
  var categories = List[Category]()
}


case class Category( val name : String
                   , val children : List[Category] = List()
                   , val color : Color)


case class HRZone( val name       : String
                 , val lowerLmit  : Int
                 , val upperLimit : Int
                 , val color      : Color)
