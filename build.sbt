lazy val commonSettings = Seq(
  organization := "deromefintech.com",
  version := "1.0",
  scalaVersion := "2.12.3"
)

val scalaOptions = Seq(
  "-deprecation", "-Xexperimental", "-encoding", "UTF-8", "-feature", "-language:_"
)

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    name := "lcboViewer",
    scalacOptions ++= scalaOptions,
    libraryDependencies ++= {
      val liftVersion = "3.0.1"
      Seq(
        "io.circe" %% "circe-core",
        "io.circe" %% "circe-generic",
        "io.circe" %% "circe-parser"
      ).map(_ % "0.8.0") ++ Seq(
        "org.scala-lang"    % "scala-compiler"      % scalaVersion.value,
        "org.scala-lang"    % "scala-reflect"       % scalaVersion.value,
        "org.typelevel" %% "cats" % "0.9.0",
        "net.liftweb"     %% "lift-webkit" % liftVersion % "compile" withSources(),
        "net.liftweb"     %% "lift-mapper" % liftVersion % "compile->default" withSources(),
        "net.liftweb"     %% "lift-squeryl-record" % liftVersion % "compile->default" withSources(), // Record interface to RDBMS,
        "postgresql"        % "postgresql"          % "9.1-901.jdbc4",
        "org.scalactic" %% "scalactic" % "3.0.1",
        "org.scalatest" %% "scalatest" % "3.0.1" % "test",
        "org.skinny-framework" %% "skinny-http-client" % "2.5.1",
        "org.eclipse.jetty" % "jetty-webapp"        % "8.1.17.v20150415"  % "container,test",
        "org.eclipse.jetty" % "jetty-plus"          % "8.1.17.v20150415"  % "container,test", // For Jetty Config
        "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar"),
        "org.specs2"        %% "specs2-core"             % "4.0.1"           % Test,
        "ch.qos.logback"    % "logback-classic"     % "1.1.3"
      )
    },
    resolvers ++= Seq(
      "snapshots"     at "https://oss.sonatype.org/content/repositories/snapshots",
      "staging"       at "https://oss.sonatype.org/content/repositories/staging",
      "Artima Maven Repository" at "http://repo.artima.com/releases",
      "releases"      at "https://oss.sonatype.org/content/repositories/releases")
  )

unmanagedResourceDirectories in Test <+= baseDirectory { _ / "src/main/webapp" }

javaOptions in run += "-Xmn2G -XX:+UseG1GC -XX:MaxGCPauseMillis=200 -XX:+PrintGC -XX:+PrintGCTimeStamps"

enablePlugins(JettyPlugin)  // so we can do jetty:start jetty:stop in sbt https://github.com/earldouglas/xsbt-web-plugin/blob/master/docs/2.0.md

containerPort := 8090  // applicable when running from sbt, not with the jetty container plug-in in IDEA (which uses 8080 and not this variable).
