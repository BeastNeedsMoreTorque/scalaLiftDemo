name := "lcboViewer"
version := "1.0"
scalaVersion := "2.11.7"
organization := "ORG"
resolvers ++= Seq("snapshots"     at "https://oss.sonatype.org/content/repositories/snapshots",
                  "staging"       at "https://oss.sonatype.org/content/repositories/staging",
                  "releases"      at "https://oss.sonatype.org/content/repositories/releases"
                 )
unmanagedResourceDirectories in Test <+= baseDirectory { _ / "src/main/webapp" }
// Xcheckinit should be off in prod builds (too much run-time overhead).
scalacOptions ++= Seq("-deprecation", "-explaintypes", "-feature", "-unchecked", "-Xfatal-warnings", "-Xcheckinit", "-Dusejavacp=true")
ideaExcludeFolders += ".idea"
ideaExcludeFolders += ".idea_modules"

javaOptions in run += "-Xmn2G -XX:+UseG1GC -XX:MaxGCPauseMillis=200 -XX:+PrintGC -XX:+PrintGCTimeStamps"
// log4j/slf4j is not standard for liftweb so we use ch.qos.logback (https://www.assembla.com/wiki/show/liftweb/Logging)
// We use postgresql in place of simple DB    "com.h2database"    % "h2"                  % "1.3.167"

libraryDependencies ++= {
  val liftVersion = "2.6.2"
  val squeryl = "net.liftweb" %% "lift-squeryl-record" % liftVersion % "compile->default" withSources() // Record interface to RDBMS
  val scalaCompiler = "2.11.7"

  Seq(
    "org.scala-lang"    % "scala-compiler"      % scalaCompiler,
    "org.scala-lang"    % "scala-reflect"       % scalaCompiler,
    "net.liftweb"     %% "lift-webkit" % liftVersion % "compile" withSources(),
    "net.liftweb"     %% "lift-mapper" % liftVersion % "compile->default" withSources(),
    "net.liftweb"     %% "lift-wizard" % liftVersion % "compile->default" withSources(),
    squeryl,
  "net.liftmodules" %% "lift-jquery-module_2.6" % "2.8" withSources())
}
libraryDependencies ++= Seq (
    "postgresql"        % "postgresql"          % "9.1-901.jdbc4",
    "org.scalatest"     % "scalatest_2.11"      % "2.2.6"            % "test",
    "org.apache.httpcomponents" % "httpclient"  % "4.5.2",
    "org.eclipse.jetty" % "jetty-webapp"        % "8.1.7.v20120910"  % "container,test",
    "org.eclipse.jetty" % "jetty-plus"          % "8.1.7.v20120910"  % "container,test", // For Jetty Config
    "org.specs2"        %% "specs2"             % "2.3.12"           % "test",
    "ch.qos.logback"    % "logback-classic"     % "1.0.13"
  )
//"org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar"),

enablePlugins(JettyPlugin)  // so we can do jetty:start jetty:stop in sbt https://github.com/earldouglas/xsbt-web-plugin/blob/master/docs/2.0.md
containerPort := 8080  // applicable when running from sbt, not with the jetty container plug-in in IDEA.
// Can also do triggered execution: > ~jetty:start
//enablePlugins(TomcatPlugin) // for Tomcat container instead.
