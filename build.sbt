name := "data-structure-maker"

version := "1.0"

scalaVersion := "2.11.5"

libraryDependencies ++= List(
  "org.jruby" % "jrubyparser" % "0.5.3",
  "org.jruby" % "jruby" % "1.6.0",
  "com.github.javaparser" % "javaparser-core" % "2.1.0",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test",
  "org.scalaj" %% "scalaj-http" % "1.1.5")

mainClass in (Compile, run) := Some("Optimizer")

ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

