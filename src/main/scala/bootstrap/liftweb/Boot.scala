package bootstrap.liftweb

import code.Rest.AppRest
import code.model._
import net.liftmodules.JQueryModule
import net.liftweb._
import net.liftweb.common._
import net.liftweb.http.js.jquery.JQueryArtifacts
import net.liftweb.http.{LiftRules, S, _}
import net.liftweb.mapper.{DB, _}
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util._


/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  * @see https://wiki.eclipse.org/Jetty/Howto/Configure_JNDI_Datasource, which has details for PostgreSQL
  */
class Boot {
  def boot {
    //    DefaultConnectionIdentifier.jndiName = "jdbc/liftinaction"  // can tie to servlet container configuration and thus handle different environments.
    // For example, see https://wiki.eclipse.org/Jetty/Howto/Configure_JNDI_Datasource. That is not our deployment strategy here, we use props files instead
    // (for dev that is main/resources/default.props).
    if (!DB.jndiJdbcConnAvailable_?) {

      val vendor =
        new StandardDBVendor(// user and password will be empty boxes if not specified in default.props as they are considered optional by StandardDBVendor
          Props.get("db.driver", "org.selectvendor.Driver"), // intentionally bad to force a decision in props files.
          Props.get("db.url", "jdbc:vendordbname-perhaps://localhost:5432"), // intentionally bad to force a decision in props files.
          Props.get("db.user"),
          Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(util.DefaultConnectionIdentifier, vendor)
    }

    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    Schemifier.schemify(true, Schemifier.infoF _, User, DBProduct, UserProduct) // last args represent our ORM table classes.

    // where to search snippet
    LiftRules.addToPackages("code")

    // Build SiteMap
    def sitemap = SiteMap(
      Menu.i("Home") / "index" >> User.AddUserMenusAfter, // the simple way to declare a menu

      // more complex because this menu allows anything in the
      // /static path to be visible
      Menu(Loc("Static", Link(List("static"), true, "/static/index"),
        "Static Content")))

    def sitemapMutators = User.sitemapMutator

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMapFunc(() => sitemapMutators(sitemap))

    //Init the jQuery module, see http://liftweb.net/jquery for more information.
    LiftRules.jsArtifacts = JQueryArtifacts
    JQueryModule.InitParam.JQuery = JQueryModule.JQuery191
    JQueryModule.init()

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // Sends user location to determine closest store and possibly additional services.
    LiftRules.dispatch.append(AppRest)

    // What is the function to test if a user is logged in?
    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)

  }
}
