val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "decoupled-by-default",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.typelevel" %% "cats-free" % "2.12.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "2.5.3",
  )
