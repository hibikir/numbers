name := "numbers"

version := "1.0"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-feature","-language:implicitConversions")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.scalacheck"%% "scalacheck" % "1.12.2" % "test"

