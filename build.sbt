
name := "data-structure-maker"

version := "1.0"

scalaVersion := "2.11.5"

resolvers ++= List(
  Resolver.sonatypeRepo("releases"),
  "Finatra Repo" at "http://twitter.github.com/finatra",
  "Twitter Maven" at "http://maven.twttr.com"
)

libraryDependencies ++= List(
  "com.github.javaparser" % "javaparser-core" % "2.1.0",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test",
  "org.scalaj" %% "scalaj-http" % "1.1.5",
  "com.twitter" %% "finatra" % "2.0.0",
  "com.twitter.finatra" %% "finatra-http" % "2.0.0.M2",
  "com.twitter.finatra" %% "finatra-logback" % "2.0.0.M2",
  "com.twitter.inject" %% "inject-server" % "2.0.0.M2" % "test",
  "com.twitter.inject" %% "inject-app" % "2.0.0.M2" % "test",
  "com.twitter.inject" %% "inject-core" % "2.0.0.M2" % "test",
  "com.twitter.inject" %% "inject-modules" % "2.0.0.M2" % "test",
  "com.twitter.finatra" %% "finatra-http" % "2.0.0.M2" % "test" classifier "tests",
  "com.twitter.inject" %% "inject-server" % "2.0.0.M2" % "test" classifier "tests",
  "com.twitter.inject" %% "inject-app" % "2.0.0.M2" % "test" classifier "tests",
  "com.twitter.inject" %% "inject-core" % "2.0.0.M2" % "test" classifier "tests",
  "com.twitter.inject" %% "inject-modules" % "2.0.0.M2" % "test" classifier "tests"
)

mainClass in (Compile, run) := Some("Optimizer")

ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

