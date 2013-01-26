name         := "trainingslog"

version      := "0.0.1"

organization := "net.fstaals"

scalaVersion := "2.9.1"

resolvers ++= Seq("snapshots"     at "http://oss.sonatype.org/content/repositories/snapshots",
                "releases"        at "http://oss.sonatype.org/content/repositories/releases"
                )

seq(com.github.siasia.WebPlugin.webSettings :_*)

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= {
  val liftVersion = "2.5-M3"
  Seq(
    "net.liftweb"             %% "lift-webkit"        % liftVersion            % "compile"
  , "net.liftweb"             %% "lift-mapper"        % liftVersion            % "compile"
  , "net.liftweb"             %% "lift-textile"       % "2.4"                  % "compile->default"
  , "net.liftmodules"         %% "widgets"            % (liftVersion + "-1.1") % "compile->default"
  , "net.liftmodules"         %% "lift-jquery-module" % (liftVersion + "-2.0")
  , "org.eclipse.jetty"       % "jetty-webapp"        % "8.1.7.v20120910"      % "container,test"
  , "org.eclipse.jetty.orbit" % "javax.servlet"       % "3.0.0.v201112011016"  % "container,test" artifacts Artifact("javax.servlet", "jar", "jar")
  , "ch.qos.logback"          % "logback-classic"     % "1.0.6"
  , "org.specs2"              %% "specs2"             % "1.12.1"               % "test"
  , "com.h2database"          % "h2"                  % "1.3.167"
  , "org.scalaj"              %% "scalaj-time"        % "0.6"
  )
}



// for the sportstracker library stuff
unmanagedBase <<= baseDirectory { base => base / "custom_lib" }
