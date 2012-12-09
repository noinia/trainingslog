package net.fstaals.tl.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import Helpers._


import net.fstaals.tl.model._

import java.io.File


class SyncActivities extends {

  type Path  = String
  type Error = String

  var fails        = List[(Path,Error)]()
  var existingActs = List[Activity]()
  var newActs      = List[Activity]()

  def sync = {
    var path : Option[String] = None

    def process() = path match {
      case Some(p) => try { addFiles((new File(p)).listFiles() map {_.getPath})
                      } catch { case _ => {S.error("Error opening file")}}
      case None    => {S.error("No such directory.")}
    }

    "name=path"   #> SHtml.onSubmit({p => path = Some(p)}) &
    "type=submit" #> SHtml.onSubmitUnit(process)
  }


  def newActivities      = "div *" #> listActs(newActs)
  def existingActivities = "div *" #> listActs(existingActs)

  def listActs(as: List[Activity]) = ".actList *" #> (as map {a =>
    "li *" #> a.name.get
  })

  def failures = "div *" #> (fails map {"li *" #> _._1})

  type AddActResult = Either[Either[(Path,Error),Activity],Activity]
  // Left(Left((path,error))) = error
  // Left(Right(a))           = existing activity
  // Right                    = new Activity

  def addFile(path: Path) : AddActResult = {
    Activity.existingFromPath(path) match {
      case Full(a)        => Left(Right(a))
      case Failure(e,_,_) => Left(Left((path,e)))
      case Empty      => Activity.fromPath(path) map {a => (a,a.validate)} match {
        case Full((a,Nil))  => {a.save ; Right(a)}
        case Full((_,es))   => Left(Left((path,es mkString "\n")))
        case Failure(e,_,_) => Left(Left((path,e)))
        case Empty          => Left(Left((path,"Unknown error.")))
      }
    }
  }

  def addFiles(files : Seq[String]) = {
    val res           = files map addFile
    val start         = (List[(Path,Error)](), List[Activity](), List[Activity]())
    val (ers,exs,nas) = res.foldRight(start)({case (x,(es,as,ns)) => x match {
       case Left(Left(e))  => (e::es,as,ns)
       case Left(Right(a)) => (es,a::as,ns)
       case Right(a)       => (es,as,a::ns)
    }})

    fails        = ers
    existingActs = exs
    newActs      = nas

    println(fails)
    println(existingActs)
    println(newActs)

    S.notice("#fails: %d, #existing: %d, #new: %d".format(fails.size,existingActs.size,newActs.size))
  }
}

object AddActivity {

  def render = {
    var af : ActivityFile = ActivityFile("dummy")

    def process() = {
      if (af.hasData)
        add(af)
      else {
        S.notice("Failed to load ActivityFile")
      }
    }

    "name=path"   #> SHtml.onSubmit(ActivityFile.fromPath) &
    "type=submit" #> SHtml.onSubmitUnit(process)
  }

  def add(af : ActivityFile) = User.currentUser match {
    case Full(u) => { val a = Activity.fromActivityFile(af).owner(u)
                     a.validate match {
                       case Nil => {
                         a.save
                         S.notice("Entry added!")
                         S.redirectTo("view/" + a.id.get.toString)
                       }
                       case x => S.error(x)
                     }}
    case _       => S.error("error not logged in")
  }


}
