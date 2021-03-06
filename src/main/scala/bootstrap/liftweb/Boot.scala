package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import js.jquery.JQueryArtifacts
import sitemap._
import mapper._

import net.fstaals.tl.TLSiteMap
import net.fstaals.tl.model._


import net.liftmodules.JQueryModule

import net.liftmodules.widgets.flot._
import net.liftmodules.widgets.autocomplete.AutoComplete

import net.liftmodules.widgets.tablesorter.TableSorter

import net.liftmodules.widgets.calendars._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {


  def boot {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor =
    new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
                         Props.get("db.url") openOr
                         "jdbc:h2:trainingslog;AUTO_SERVER=TRUE",
                         Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    Schemifier.schemify(true, Schemifier.infoF _,
                        User, HRZone,
                        Activity, Exercise,
                        Sport,
                        Tag, ActivityTags)

    // where to search snippet
    LiftRules.addToPackages("net.fstaals.tl")

    def sitemapMutators = User.sitemapMutator

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMapFunc(() => sitemapMutators(TLSiteMap.sitemap))

    //Init the jQuery module, see http://liftweb.net/jquery for more information.
    LiftRules.jsArtifacts = JQueryArtifacts
    JQueryModule.InitParam.JQuery=JQueryModule.JQuery172
    JQueryModule.init()

    // use flot
    Flot.init
    // init the table sorter
    TableSorter.init

    // Calendar
    CalendarMonthView.init

    // autocomplete
    AutoComplete.init

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)

    // make notice and warnings auto fade away
    LiftRules.noticesAutoFadeOut.default.set( (notices: NoticeType.Value) => {
        notices match {
          case NoticeType.Notice  => Full((2 seconds, 2 seconds))
          case NoticeType.Warning => Full((4 seconds, 2 seconds))
          case NoticeType.Error   => Empty // don't fade
        }
     }
    )

  }





}
