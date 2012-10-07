package net.fstaals.model

import org.joda.time.DateTime

class Activity {

  var name         = "unnamed"
  var start        : DateTime
  var end          : DateTime
  var exercises    = List[Exercise]()
  var activityFile = None
  var sport        : Sport
  var category     : Category

}


class Exercise {
  var name  = "unnamed"
  var rpe  : RPE
  var description = ""
  var start : DateTime
  var end   : DateTime
}
