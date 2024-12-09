val scala3Version = "3.5.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "prop-testing",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.18.1" % Test,
    // Enable ScalaCheck
    testFrameworks += new TestFramework("org.scalacheck.ScalaCheckFramework"),

    // Automatically import PropertyTestUtils and other common test dependencies in the Test console
    Test / console / initialCommands := """
      import org.scalacheck.Prop._
      import org.scalacheck.Test.Parameters
      import BinarySearchTreeSpec._, BinarySearchTreeSpec.given
    """
  )
