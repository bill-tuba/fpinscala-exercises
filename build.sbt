name := "function programming in scala exercises"

scalaVersion := "2.10.2"

organization := "Bill Adams"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
  "org.scalamock" %% "scalamock-scalatest-support" % "3.0.1" % "test"
)

// append options passed to the Scala compiler
scalacOptions ++= Seq("-deprecation", "-unchecked")
