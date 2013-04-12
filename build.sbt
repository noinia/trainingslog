name         := "trainingslog"

version      := "0.0.1"

organization := "net.fstaals"

scalaVersion := "2.10.1"

resolvers ++= Seq("snapshots"     at "http://oss.sonatype.org/content/repositories/snapshots",
                "releases"        at "http://oss.sonatype.org/content/repositories/releases"
                )

seq(com.github.siasia.WebPlugin.webSettings :_*)

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= {
  val liftVersion = "2.5-RC2"
  Seq(
    "net.liftweb"             %% "lift-webkit"            % liftVersion            % "compile"
  , "net.liftweb"             %% "lift-mapper"            % liftVersion            % "compile"
  , "net.liftmodules"         %% "textile_2.5"            % "1.3"                  % "compile->default"
  , "net.liftmodules"         %% "widgets_2.5"            % "1.3"                  % "compile->default"
  , "net.liftmodules"         %% "lift-jquery-module_2.5" % "2.3"
  , "org.eclipse.jetty"       %  "jetty-webapp"           % "8.1.7.v20120910"      % "container,test"
  , "org.eclipse.jetty.orbit" %  "javax.servlet"          % "3.0.0.v201112011016"  % "container,test" artifacts Artifact("javax.servlet", "jar", "jar")
  , "ch.qos.logback"          %  "logback-classic"        % "1.0.6"
  , "org.specs2"              %% "specs2"                 % "1.14"                 % "test"
  , "com.h2database"          %  "h2"                     % "1.3.167"
  , "com.github.nscala-time"  %% "nscala-time"            % "0.4.0"
  , "org.codehaus.groovy"     %  "groovy"                 % "2.1.2"
  )
}



// for the sportstracker library stuff
unmanagedBase <<= baseDirectory { base => base / "custom_lib" }


seq(runModeSettings : _*)

RunModeKeys.jettyVersion in Production := JettyVersion.Jetty7Plus

// // javacOptions.in(Production) ++= Seq("-target", "1.6")
// javacOptions ++= Seq("-target", "1.6")
