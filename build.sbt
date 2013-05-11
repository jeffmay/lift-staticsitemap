name := "staticsitemap"

organization := "net.liftmodules"

version := "0.8-SNAPSHOT"

liftVersion <<= liftVersion ?? "2.5-SNAPSHOT"

liftEdition <<= liftVersion apply { _.substring(0,3) }

name <<= (name, liftEdition) { (n, e) =>  n + "_" + e }
 
scalaVersion := "2.10.0"

crossScalaVersions := Seq("2.10.0", "2.9.2", "2.9.1-1", "2.9.1")

scalacOptions ++= Seq("-unchecked", "-deprecation")

resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

libraryDependencies <++= liftVersion { v =>
  "net.liftweb" %% "lift-webkit" % v % "compile->default" ::
  "org.scalatest" %% "scalatest" % v % "test" ::
  Nil
}    

libraryDependencies <++= scalaVersion { sv => 
  (sv match { 
        case "2.9.2" | "2.9.1" | "2.9.1-1" => "org.specs2" %% "specs2" % "1.12.3" % "test"
        case "2.10.0" => "org.specs2" %% "specs2" % "1.13" % "test"
   }) ::
  Nil
}

