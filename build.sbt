name := "numbers"

version := "1.0"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-feature","-language:implicitConversions")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.2.1" % "test"

libraryDependencies += "org.scalacheck"%% "scalacheck" % "1.11.5" % "test"

