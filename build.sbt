name := "sjq"

scalaVersion := "2.11.8"

val argonaut = "6.2-M1"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "0.4.2",
  "org.scalaz" %% "scalaz-core" % "7.2.7",

  "io.argonaut" %% "argonaut" % argonaut,

  "org.specs2" %% "specs2-core" % "3.8.9" % "test"
)

//Parser not thread-safe
parallelExecution in Test := false