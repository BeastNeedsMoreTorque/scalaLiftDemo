name := "lcboViewer"

version := "1.0"

scalaVersion := "2.11.8"

organization := "ORG"

resolvers ++= Seq("snapshots"     at "https://oss.sonatype.org/content/repositories/snapshots",
                  "staging"       at "https://oss.sonatype.org/content/repositories/staging",
                  "Artima Maven Repository" at "http://repo.artima.com/releases",
                  "releases"      at "https://oss.sonatype.org/content/repositories/releases")

unmanagedResourceDirectories in Test <+= baseDirectory { _ / "src/main/webapp" }

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_"
)

ideaExcludeFolders += ".idea"

ideaExcludeFolders += ".idea_modules"

javaOptions in run += "-Xmn2G -XX:+UseG1GC -XX:MaxGCPauseMillis=200 -XX:+PrintGC -XX:+PrintGCTimeStamps"

lazy val compileScalastyle = taskKey[Unit]("compileScalastyle")

compileScalastyle := org.scalastyle.sbt.ScalastylePlugin.scalastyle.in(Compile).toTask("").value

(compile in Compile) <<= (compile in Compile) dependsOn compileScalastyle

val circeVersion = "0.6.1"

val liftVersion = "2.6.2"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies ++= {
  Seq(
    "org.scala-lang"    % "scala-compiler"      % scalaVersion.value,
    "org.scala-lang"    % "scala-reflect"       % scalaVersion.value,
    "org.typelevel" %% "cats" % "0.8.1",
    "net.liftweb"     %% "lift-webkit" % liftVersion % "compile" withSources(),
    "net.liftweb"     %% "lift-mapper" % liftVersion % "compile->default" withSources(),
    "net.liftweb"     %% "lift-wizard" % liftVersion % "compile->default" withSources(),
    "net.liftweb"     %% "lift-squeryl-record" % liftVersion % "compile->default" withSources(), // Record interface to RDBMS,
    "net.liftmodules" %% "lift-jquery-module_2.6" % "2.8" withSources(),
    "postgresql"        % "postgresql"          % "9.1-901.jdbc4",
    "org.scalactic" % "scalactic_2.11" % "3.0.1",
    "org.scalatest" % "scalatest_2.11" % "3.0.1" % "test",
    "org.skinny-framework" %% "skinny-http-client" % "2.2.0",
    "org.eclipse.jetty" % "jetty-webapp"        % "8.1.17.v20150415"  % "container,test",
    "org.eclipse.jetty" % "jetty-plus"          % "8.1.17.v20150415"  % "container,test", // For Jetty Config
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar"),
    "org.specs2"        %% "specs2"             % "2.3.12"           % Test,
    "ch.qos.logback"    % "logback-classic"     % "1.1.3"
  )
}

enablePlugins(JettyPlugin)  // so we can do jetty:start jetty:stop in sbt https://github.com/earldouglas/xsbt-web-plugin/blob/master/docs/2.0.md

containerPort := 8090  // applicable when running from sbt, not with the jetty container plug-in in IDEA (which uses 8080 and not this variable).
