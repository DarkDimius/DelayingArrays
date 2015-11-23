
import sbt._
import Keys._
import java.io.File
import pl.project13.scala.sbt.JmhPlugin



object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq (
    name := "delaying-arrays",
    organization := "me.d-d",
    version := "0.1",
    scalaVersion := "2.11.5",
    scalacOptions ++= Seq("-deprecation", "-optimise"),
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter" % "0.6" % "test"
      , "org.scalatest" %% "scalatest" % "2.1.5" % "test"
      , "org.scala-lang" % "scala-compiler" % "2.11.1" % "provided",
      "org.scalacheck" %% "scalacheck" % "1.11.4" % "test"
    ),
    initialCommands in console := "import me.d_d.delaying._",
    logBuffered := false)
}


object ScalaBlitzBuild extends Build {
  
  def quote(s: Any) = {
    if (scala.util.Properties.isWin) "\"" + s.toString + "\""
    else s.toString
  }


  /* tasks and settings */
   
  val javaCommand = TaskKey[String](
    "java-command",
    "Creates a java vm command for launching a process."
  )
  
  val javaCommandSetting = javaCommand <<= (
    dependencyClasspath in Compile,
    artifactPath in (Compile, packageBin),
    artifactPath in (Test, packageBin),
    packageBin in Compile,
    packageBin in Test
  ) map {
    (dp, jar, testjar, pbc, pbt) => // -XX:+UseConcMarkSweepGC  -XX:-DoEscapeAnalysis -XX:MaxTenuringThreshold=12 -verbose:gc -XX:+PrintGCDetails 
    val sep = java.io.File.pathSeparator
    val javacommand = "java -Xmx4096m -Xms4096m -XX:+UseCondCardMark -server -cp %s%s%s%s%s".format(
      dp.map(x => quote(x.data)).mkString(sep),
      sep,
      quote(jar),
      sep,
      quote(testjar)
    )
    javacommand
  }
  
  val benchTask = InputKey[Unit](
    "bench",
    "Runs a specified benchmark."
  ) <<= inputTask {
    (argTask: TaskKey[Seq[String]]) =>
    (argTask, javaCommand) map {
      (args, jc) =>
      val javacommand = jc
      val comm = javacommand + " " + args.mkString(" ")
      println("Executing: " + comm)
      comm!
    }
  }
  
  val benchVerboseTask = InputKey[Unit](
    "vbench",
    "Runs a specified benchmark in a verbose mode."
  ) <<= inputTask {
    (argTask: TaskKey[Seq[String]]) =>
    (argTask, javaCommand) map {
      (args, jc) =>
      val javacommand = jc
      val verboseopts = "-XX:+UnlockDiagnosticVMOptions -XX:+PrintCompilation -XX:+PrintAssembly -XX:PrintAssemblyOptions=hsdis-print-bytes -Xbatch -verbose:gc -Xprof"
      val comm = javacommand + " " + verboseopts + " " + args.mkString(" ")
      println("Executing: " + comm)
      comm!
    }
  }
  /* projects */

  //lazy val scalameter = RootProject(uri("git://github.com/axel22/scalameter.git"))
  
  lazy val root = Project(
    "root",
    file("."),
    settings = BuildSettings.buildSettings ++ Seq(benchTask, javaCommandSetting, benchVerboseTask)
  ) enablePlugins(JmhPlugin) dependsOn ()

}

