
scalaVersion := "2.13.8"
name := "RestAPI"
organization := "misza"
version := "1.0"
val AkkaVersion = "2.6.8"
val AkkaHttpVersion = "10.2.9"

libraryDependencies ++= Seq("org.scalactic" %% "scalactic" % "3.2.10",
                            "org.scalatest" %% "scalatest" % "3.2.10" % "test",
                            "org.xerial" % "sqlite-jdbc" % "3.36.0.3",
                            "com.typesafe.slick" %% "slick" % "3.3.3",
                            "org.slf4j" % "slf4j-nop" % "1.6.4",
                            "com.typesafe.slick" %% "slick-hikaricp" % "3.3.3",
                            "com.lihaoyi" %% "os-lib" % "0.8.0",
                            "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
                            "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
                            "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
                            "com.github.jwt-scala" %% "jwt-core" % "9.0.5",
                            "io.jvm.uuid" %% "scala-uuid" % "0.3.1")