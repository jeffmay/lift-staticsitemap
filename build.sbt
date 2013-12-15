// Name and Version Info
////////////////////////

name := "staticsitemap"

organization := "net.liftmodules"

version := "0.8-SNAPSHOT"

liftVersion <<= liftVersion ?? "2.6-M2"

liftEdition <<= liftVersion apply { _.substring(0,3) }

name <<= (name, liftEdition) { (n, e) =>  n + "_" + e }

scalaVersion := "2.10.3"

crossScalaVersions := Seq("2.10.2", "2.10.1", "2.10.0")

scalacOptions ++= Seq("-unchecked", "-deprecation")

// Resolvers
////////////

resolvers += "CB Central Mirror" at "http://repo.cloudbees.com/content/groups/public"

resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

// Dependencies
///////////////

libraryDependencies <++= (liftVersion, scalaVersion) {
  (liftV, scalaV) => Seq(
    "net.liftweb" %% "lift-webkit" % liftV % "compile->default" withSources(),
    "net.liftweb" %% "lift-testkit" % liftV % "compile->default" withSources(),
    "javax.servlet" % "servlet-api" % "2.5" % "provided" withSources(),
    scalaV match {
      case "2.9.2" | "2.9.1" | "2.9.1-1" => "org.specs2" %% "specs2" % "1.12.3" % "test" withSources()
      case _ => "org.specs2" %% "specs2" % "1.13" % "test" withSources()
    },
    "org.specs2" %% "specs2" % "1.13" % "test" withSources(),
    "org.scalatest" %% "scalatest" % "1.9.1" % "test" withSources()
  )
}

// Resources
////////////

unmanagedResourceDirectories in Test <+= baseDirectory { _ / "src/test/webapp" }
