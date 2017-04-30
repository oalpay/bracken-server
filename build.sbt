name := "BrackenApp"

organization := "bracken"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

resolvers += "Liftmodules repo" at "https://repository-liftmodules.forge.cloudbees.com/release"

resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

resolvers += "Sonatype-snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

{
  val liftVersion = "2.6-M2"
  libraryDependencies ++= Seq(
    "net.liftweb" %% "lift-mongodb-record" % liftVersion,
    "net.liftmodules" %% "mongoauth_2.5" % "0.4",
    "ch.qos.logback" % "logback-classic" % "1.0.0",
    "org.scalatest" %% "scalatest" % "1.9.2" % "test",
    "org.eclipse.jetty" % "jetty-webapp" % "7.1.0.RC1" % "container",
	"org.scalaz" %% "scalaz-core" % "6.0.4",
	"org.ocpsoft.prettytime" % "prettytime" % "3.1.0.Final",
	"com.foursquare" %% "rogue-field"         % "2.2.0" intransitive(),
	"com.foursquare" %% "rogue-core"          % "2.2.0" intransitive(),
	"com.foursquare" %% "rogue-lift"          % "2.2.0" intransitive(),
	"com.foursquare" %% "rogue-index"         % "2.2.0" intransitive()
  )
}

scalacOptions ++= Seq("-deprecation", "-unchecked")

seq(jsSettings : _*)
	
(includeFilter in (Compile, JsKeys.js) := "*.jsm")

(compile in Compile) <<= compile in Compile dependsOn (JsKeys.js in Compile)

seq(lessSettings : _*)

(includeFilter in (Compile, LessKeys.less) := "*styles.less")

(compile in Compile) <<= compile in Compile dependsOn (LessKeys.less in Compile)

(sourceDirectory in (Compile, LessKeys.less)) <<= 
	(sourceDirectory in Compile)(_ / "less")

seq(webSettings :_*)

// add managed resources, where less and closure publish to, to the webapp
(webappResources in Compile) <+= (resourceManaged in Compile)
