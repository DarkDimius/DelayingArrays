name := "DelayingArrays"

enablePlugins(JmhPlugin)
//mainClass in (Jmh, run) := Some("delaying.jmh.CustomRunnerApp")

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6"
)
