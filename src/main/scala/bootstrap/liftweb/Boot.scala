package bootstrap.liftweb

import code.Rest.AppRest
import code.model._
import net.liftmodules.JQueryModule
import net.liftweb._
import net.liftweb.common._
import net.liftweb.db.StandardDBVendor
import net.liftweb.http.js.jquery.JQueryArtifacts
import net.liftweb.http.{LiftRules, _}
import net.liftweb.sitemap.Loc._
import net.liftweb.sitemap._
import net.liftweb.util._
import net.liftweb.squerylrecord.SquerylRecord
import org.squeryl.adapters.PostgreSqlAdapter
import java.sql.DriverManager
import org.squeryl.Session
import net.liftweb.mapper.DB
import net.liftweb.http.S
import net.liftweb.util.LoanWrapper
import net.liftweb.squerylrecord.RecordTypeMode._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  * @see https://wiki.eclipse.org/Jetty/Howto/Configure_JNDI_Datasource, which has details for PostgreSQL
  */
class Boot {
  // scalastyle:off method.length

  def boot: Unit = {
    //    DefaultConnectionIdentifier.jndiName = "jdbc/liftinaction"  // can tie to servlet container configuration and thus handle different environments.
    // For example, see https://wiki.eclipse.org/Jetty/Howto/Configure_JNDI_Datasource. That is not our deployment strategy here, we use props files instead
    // (for dev that is main/resources/default.props).
    val driver = Props.get("db.driver", "org.selectvendor.Driver") // intentionally bad to force a decision in props files.
    val jdbcURL = Props.get("db.url", "jdbc:vendordbname-perhaps://localhost:5432") // intentionally bad to force a decision in props files.
    if (!DB.jndiJdbcConnAvailable_?){  // for Lift's user
      // user and password will be empty boxes if not specified in default.props as they are considered optional by StandardDBVendor
      val vendor = new StandardDBVendor(
        driver,
        jdbcURL,
        Props.get("db.user"),
        Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_!)
      DB.defineConnectionManager(util.DefaultConnectionIdentifier, vendor)
    }

    // for db usage outside of Lift's User.
    Class.forName(driver)
    SquerylRecord.initWithSquerylSession(  Session.create(DriverManager.getConnection(jdbcURL), new PostgreSqlAdapter) )

    if(Props.devMode) {
      inTransaction {
        code.model.MainSchema.printDdl
      }
    }

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

    // Init the jQuery module, see http://liftweb.net/jquery for more information.
    LiftRules.jsArtifacts = JQueryArtifacts
    JQueryModule.InitParam.JQuery = JQueryModule.JQuery191
    JQueryModule.init()

    val spinnyImmageName= "ajax-loader"
    // Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show(spinnyImmageName).cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide(spinnyImmageName).cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // Sends user location to determine closest store and possibly additional services.
    AppRest.init()

    // What is the function to test if a user is logged in?
    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    LiftRules.contentSecurityPolicyViolationReport = { violation => Empty } // big cheat
    // @see https://groups.google.com/forum/#!topic/liftweb/S4La8Ut5R90
    // should address real issues with something along those lines
    /* LiftRules.securityRules = () => {
      SecurityRules(content = Some(ContentSecurityPolicy(
        styleSources = List(
          ContentSourceRestriction.All
        ),
        defaultSources = List(
          ContentSourceRestriction.All
        ),

        fontSources = List(
          ContentSourceRestriction.All
        ),
        scriptSources = List(
          ContentSourceRestriction.All
        )
      )))
    } */

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    // Make a transaction span the whole HTTP request (Squeryl way that is not the traditional method from Liftweb)
    // As per Lift Cookbook, requests are inTransaction scope (Squeryl terminology).
    S.addAround(new LoanWrapper {
      override def apply[T](f: => T): T = {
        val resultOrExcept = inTransaction {
          try {
            Right(f)
          } catch {
            case e: LiftFlowOfControlException => Left(e)
          }
        }

        resultOrExcept match {
          case Right(result) => result
          case Left(except) => throw except
        }
      }
    })
    Product.load()
    Store.load()
  }
  // scalastyle:on method.length

}
