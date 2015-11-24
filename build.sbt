name := "duplicitous"
organization := "com.evolvedbinary"
version := "1.0"

organizationName := "Evolved Binary Ltd"
description := "Duplicitous rewrites the content of a document(s) whilst maintaining the structure"

scalaVersion := "2.11.7"

libraryDependencies ++= {

  val scalazV = "7.1.5"

  Seq(
    "com.github.scopt" %% "scopt" % "3.3.0",
    "com.madgag" %% "scala-arm" % "1.3.4",
    "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3-1",
    "org.scalaz" %% "scalaz-core" % scalazV,
    "org.scalaz" %% "scalaz-concurrent" % scalazV,
    "org.scalaz.stream" %% "scalaz-stream" % "0.8",
    "org.clapper" %% "grizzled-slf4j" % "1.0.2",
    "com.fasterxml" % "aalto-xml" % "1.0.0",

    "org.slf4j" % "slf4j-simple" % "1.7.13" % "runtime"
  )
}
