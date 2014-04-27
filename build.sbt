name := "BDI transactor"

version := "1"

scalaVersion := "2.10.3"

libraryDependencies ++= List(
  "org.scalatest" % "scalatest_2.10" % "2.1.4" % "test"
, "org.json4s"    %% "json4s-jackson" % "3.2.9"
)

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

